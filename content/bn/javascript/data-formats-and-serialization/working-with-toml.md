---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:30:46.432713-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
