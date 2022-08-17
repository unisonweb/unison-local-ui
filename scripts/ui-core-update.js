#!/usr/bin/env node

const {
  getLatestUICoreSha,
  elmGitInstall,
  replaceElmGitSha,
} = require("./ui-core");

getLatestUICoreSha()
  .then(replaceElmGitSha)
  .then(() => elmGitInstall());
