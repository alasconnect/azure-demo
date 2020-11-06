
# Database targets ------------------------------------------------------------

.PHONY: db-setup
db-setup:
	PGPASSWORD=admin psql -U admin -h localhost -d postgres < db_schema.sql
	moo-postgresql upgrade --config-file moo.cfg

.PHONY: db-upgrade
db-upgrade:
	moo-postgresql upgrade --config-file moo.cfg

.PHONY: db-drop
db-drop:
	PGPASSWORD=admin psql -U admin -h localhost -d postgres < db_schema_destroy.sql

.PHONY: db-shell
db-shell:
	PGPASSWORD=scp psql -U scp -h localhost -d scp

.PHONY: new-migration
new-migration:
	moo-postgresql new --config-file moo.cfg @$
