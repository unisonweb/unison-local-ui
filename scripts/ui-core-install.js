#!/usr/bin/env node

const { elmGitInstall, replaceElmGitSha } = require("./ui-core");

const shaArg = process.argv.slice(2)[0];

Promise.resolve(shaArg)
  .then((sha) => (sha ? replaceElmGitSha(sha) : sha))
  .then(() => elmGitInstall());
