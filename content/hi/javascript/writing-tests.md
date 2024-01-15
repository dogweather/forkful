---
title:                "प्रोग्रामिंग कोड लिखना"
html_title:           "Javascript: प्रोग्रामिंग कोड लिखना"
simple_title:         "प्रोग्रामिंग कोड लिखना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Kyu

Tests likhna koi optional kaam nahi hai, balki ye ek zaroori step hai code quality ko improve karne ke liye. Isse hum apne code par confidence rakhte hain aur bugs ko pehle se hi detect kar sakte hain, jisse bug fixing time reduce hojata hai.

## Kaise Kare

Unit tests banana bahut hi aasan hai. Pehle hum chai chai (chai assertion library) ka use kartein hain, phir mocha (testing framework) ka use kartein hain.

```
// chai ke saath chai chai install karna
npm install chai -D
// mocha install karna, -g flag use karke globally install karna hai
npm install mocha -g
```

Ab chai chai aur mocha ko use karke ek test file create karenge.

```
// test.js file create karke chai chai aur mocha ko import karna
var expect = require('chai').expect;
var assert = require('chai').assert;
var request = require('chai').request;
var app = require('../server.js');

// describe aur it functions use karke ek test case create karna
describe('GET /user', function() {
  it('should return a 200 response', function(done) {
    request(app)
      .get('/user')
      .expect(200, done);
  });
  it('should return an array of users', function(done) {
    request(app)
      .get('/user')
      .expect(function(res) {
        expect(res.body).to.be.an('array');
        assert.isEmpty(res.body);
      })
      .end(done);
  });
});
```

Code ke upar dekhein, chai chai ke saath chai banner use karke hum chai chai library ko import karke chai aur assert objects ko initialize kar sakte hain. Phir chai chai ka use karke chai chai banner se chai chai objects create kar sakte hain. Chai chai banner ke baad chai chai ka use karein aur expect aur assert objects ko chai chai banner se initialize karein. Phir mocha banner use karke mocha library ko import karein. Pher mocha ke functions describe aur it ka use karke hum apne test cases likh sakte hain.

## Deep Dive

Unit tests likhna humare code ko prepared rakhte hain unexpected errors aur bugs ke liye. Tests ke help se hum apne code ko robust bana sakte hain aur future mein modifications ko asaani se implement kar sakte hain. Isse humari productivity aur code quality dono hi improve hoti hai.

## Aage Dekhein

- [Chai Assertion Library](https://www.chaijs.com/) 
- [Mocha Testing Framework](https://mochajs.org/) 
- [Unit Testing in Javascript](https://www.freecodecamp.org/news/javascript-unit-testing-for-beginners-9182d13a3cc1/)