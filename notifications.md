# Notifications Overview

## Overview
flightsayer's notification systems are comprised of the following parts:

### alerts-service ([`alerts-service`](https://bitbucket.org/resilientops/alerts-service))
A database listener for new notifications. On triggers, it pulls data from the flightsayer API and forwards the data to the Notifications API

### Notifications API ([`api-notifications`](https://bitbucket.org/resilientops/api-notifications))
An HTTP web service for sending emails / SMS messages. Allows users to setup event handlers for different notifications. Uses the *email-templater* to inject data into notification templates specified by *flightsayer-templates*. A/B testing experiments can be setup here too.

### flightsayer-templates ([`@flightsayer/templates`](https://bitbucket.org/resilientops/flightsayer-templates))
A collection of Liquid templated emails (and later SMS) messages. Contains code to bootstrap default handlers on the *Notifications API*.

### email-templater ([`@flightsayer/email-templater`](https://bitbucket.org/resilientops/email-templater))
Liquid-based templater with additional flightsayer provided data formatters. Used by *Notifications API* for notification formatting as well as *flightsayer-templates* for unit testing

# Development Workflow
Best engineering practices + workflows:

## Best practices
### Local testing
1. Always pull the latest version of the code from Bitbucket
2. Create your own development branch (feature-*) `git branch new-feature`
3. Add / remove features
4. Run local validations and unit tests `npm test`

## Deploy to development (staging)
1. Revision the package using `npm version major|minor|patch` according to Semantic Versioning
2. Push changes to Bitbucket `git push`
3. Ensure that the build succeeds. 
4. Any additional steps (e.g. restart server, etc.) should be done here

## Deploy to production
1. Submit a pull request for the feature branch and add a reviewer
2. Once approved and merged, ensure the build succeeds for production
3. Any additional steps (e.g. publish to NPM, restart server, etc.) should be done here

## Add flightsayer NPM registry
You'll need flightsayer's private NPM registry in order to access/publish Notifications modules:

- Add the registry:
`npm set registry http://ec2-52-43-252-95.us-west-2.compute.amazonaws.com:4873`
- Create a user:
`npm adduser --registry http://ec2-52-43-252-95.us-west-2.compute.amazonaws.com:4873`
- Add scoped registry:
`npm set @flightsayer:registry http://ec2-52-43-252-95.us-west-2.compute.amazonaws.com:4873`

## Update email templates
Follow the development workflow for making your changes. You can verify changes to templates using MJML's try it live online

### Additional publish steps for development (staging)
1. SSH into the Docker Swarm (`swarm.internal` or `ec2-52-43-252-95.us-west-2.compute.amazonaws.com`)
2. Force a restart of the dev Notifications API: `docker service update --env-add UPDATE={whatever goes here} api-notifications-dev`
3. Send notifications to the dev server (`swarm.internal:30000`) to ensure everything works as expected

### Additional publish steps for production
1. SSH into the Docker Swarm (`swarm.internal` or `ec2-52-43-252-95.us-west-2.compute.amazonaws.com`)
2. Force a restart of the prod Notifications API: `docker service update --env-add UPDATE={whatever goes here} api-notifications`
3. Send notifications to the dev server (`swarm.internal:3000`) to ensure everything works as expected

## Add new Liquid data formatters
### Additional publish steps for development (staging)
1. Append `-dev` to the version of the package in `package.json` for *email-templater*
2. Publish the package: `npm publish`

### Additional publish steps for production
1. Remove `-dev` to the version of the package in `package.json` for *email-templater* if it exists
2. Publish the package `npm publish`

### Update `flightsayer-templates` references (dev and production)
1. Open `package.json` and locate the dev dependency for `@flightsayer/email-templater`
2. Update the located version to your published version.
3. `npm install`
4. Follow the best practices to properly test and deploy `flightsayer-templates`

### Update `api-notifications` references (dev and production)
Follow the best engineering practices for making modifications  
1. Open `package.json` and locate the dependency for `@flightsayer/templater` in `api-notifications`
2. Update the version to your published version 
3. Follow the best practices to properly test and deploy `api-notifications`