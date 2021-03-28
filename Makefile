all: igmng grafana

grafana:
	@mkdir -p ./containers/grafana_data
	@sudo chown -R 104:104 ./containers/grafana_data
	@cd containers && docker-compose up -d grafana

igmng:
	@cd containers && docker-compose up -d router db cron

kill-grafana:
	@cd containers && docker-compose stop grafana

kill-all:
	@cd containers && docker-compose stop

PHONY: all grafana igmng kill-grafana kill-all
