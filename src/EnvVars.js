"use strict";

exports.getEnv = function () {
    return (process || {}).env || {};
}