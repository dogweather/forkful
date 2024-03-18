---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:46.432713-06:00
description: "TOML, \u09AF\u09BE Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\
  \u0982\u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u0995\u09A8\
  \u09AB\u09BF\u0997 \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7 \u0997\u09A0\u09A8 \u0995\u09B0\u09BE \u0989\u099A\u09BF\
  \u09A4 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\
  \ \u0995\u09BE\u09B0\u09A3 \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.476766-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u09AF\u09BE Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\u0982\
  \u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u0995\u09A8\u09AB\
  \u09BF\u0997 \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u0997\u09A0\u09A8 \u0995\u09B0\u09BE \u0989\u099A\u09BF\u09A4\
  \ \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09C7\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\
  \ \u0995\u09BE\u09B0\u09A3 \u098F\u099F\u09BF\u2026"
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
TOML, যা Tom's Obvious, Minimal Language এর সংক্ষিপ্ত রূপ, কনফিগ ফাইলগুলি কিভাবে গঠন করা উচিত তা নির্দেশ করে। প্রোগ্রামাররা TOML এর সাথে কাজ করে কারণ এটি পড়া, লেখা এবং একটি হ্যাশ টেবিলে সুন্দরভাবে ম্যাপিং করা সহজ, যা কনফিগুরেশনের জন্য একটি যাওয়া-আসা পথ হিসেবে গণ্য হয়।

## কিভাবে:
JavaScript এ TOML এর সাথে কাজ করতে, আপনার `@iarna/toml` এর মতো একটি পারসার প্রয়োজন হবে। প্রথমে, এটি ইনস্টল করুন: `npm install @iarna/toml`। এরপর, একটি TOML স্ট্রিংকে একটি JavaScript অবজেক্টে পার্স করুন অথবা একটি JavaScript অবজেক্টকে TOML ফরম্যাটে স্ট্রিংগিফাই করুন।

```javascript
const toml = require('@iarna/toml');

// TOML স্ট্রিংকে JS অবজেক্টে পার্স করুন
const tomlStr = `
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// JS অবজেক্টকে TOML স্ট্রিং এ রূপান্তর করুন
const jsObject = {
  title: "TOML Example",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## গভীরে ডুব
TOML প্রথম 2013 সালে Tom Preston-Werner, GitHub এর একজন সহ-প্রতিষ্ঠাতা দ্বারা মুক্তি পেয়েছিল। এটি INI এর মতো অন্যান্য ফরম্যাটগুলিকে অতিক্রম করার উদ্দেশ্যে নকশা করা হয়েছিল, আরও মানসম্মত এবং পার্স করা সহজ হওয়ার মাধ্যমে। JSON এবং YAML হল বিকল্প কিন্তু অতিজটিল অথবা অতিরিক্ত নমনীয় হতে পারে। TOML এর সুবিধা হল স্থির কনফিগুরেশনে, যেখানে একটি সহজ, স্পষ্ট ফরম্যাট পছন্দসই। এর নকশা একটি হ্যাশ টেবিলে সরাসরি ম্যাপিং অনুমতি দেয়, যেখানে কী এবং মানগুলি সম্পত্তির নাম এবং তাদের মানের সাথে মিলে যায়। আরও ব্যাপক গ্রহণযোগ্যতার জন্য, বিভিন্ন ইকোসিস্টেম সমর্থনের কারণে TOML এবং অন্য ফরম্যাটের মধ্যে রূপান্তর করতে সক্ষম টুলস সমন্বিত করা প্রয়োজন হতে পারে।

## আরও দেখুন
- অফিসিয়াল TOML GitHub রিপোজিটরি: https://github.com/toml-lang/toml
- TOML বনাম YAML বনাম JSON তুলনা: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` প্যাকেজ: https://www.npmjs.com/package/@iarna/toml
