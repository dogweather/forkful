---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:58.162599-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u09AE\u09C2\
  \u09B2\u09A4 \u0997\u09C1\u0997\u09B2\u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09CD\u09AF\u09C1\u099F\
  \u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u09B8\u09B9 \u099C\
  \u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F, \u09A4\
  \u09BE\u0987 Google Apps Script \u098F\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF TOML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u098F\u0995\u099F\u09C1\u2026"
lastmod: '2024-03-17T18:47:43.551494-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u09AE\u09C2\u09B2\u09A4 \u0997\u09C1\u0997\u09B2\u09C7\
  \u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\
  \u09B0 \u09B8\u09CD\u09AF\u09C1\u099F\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B8\u09C7\u09B8 \u09B8\u09B9 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\
  \u09B0\u09BF\u09AA\u09CD\u099F, \u09A4\u09BE\u0987 Google Apps Script \u098F\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF TOML \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u098F\u0995\u099F\
  \u09C1 \u0989\u09A6\u09CD\u09AD\u09BE\u09AC\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8\u0964 Google Apps Script TOML \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982 \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09C7 \u09A8\u09BE\
  , \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\
  \u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\
  \ \u0995\u09B0\u09A4\u09C7 \u09AC\u09BE \u09AE\u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u0995\u099F\u09BF \u09B8\u09B9\u099C \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\u09B0\
  \ \u09B2\u09BF\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\n\n\u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C TOML \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AA\u09BE\u09B0\u09CD\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0996\u09BE \u09AF\u09BE\u0995."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
Google Apps Script মূলত গুগলের অ্যাপ্লিকেশনের স্যুটে অ্যাক্সেস সহ জাভাস্ক্রিপ্ট, তাই Google Apps Script এর মধ্যে সরাসরি TOML নিয়ে কাজ করা একটু উদ্ভাবনের প্রয়োজন। Google Apps Script TOML পার্সিং গ্রহণ করে না, তবে আপনি জাভাস্ক্রিপ্ট লাইব্রেরিগুলি সম্পাদন করতে বা মৌলিক প্রয়োজনের জন্য একটি সহজ পার্সার লিখতে পারেন।

একটি সহজ TOML কনফিগারেশন স্ট্রিং পার্স করার একটি উদাহরণ দেখা যাক:

```javascript
// TOML স্ট্রিং
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// একটি সহজ TOML থেকে JSON পার্সার ফাংশন
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // নতুন সেকশন
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // সহজতার জন্য eval ব্যবহার; উৎপাদন কোডে সাবধানে
      currentSection[key] = value;
    }
  });
  return result;
}

// পার্সার পরীক্ষা
var configObject = parseTOML(tomlString);
console.log(configObject);

```

`console.log` থেকে নমুনা আউটপুট একটি JSON অবজেক্টের মতো দেখাবে, যা Google Apps Script এর মধ্যে কনফিগারেশন বৈশিষ্ট্যগুলি অ্যাক্সেস করা সহজ করে তোলে:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## গভীর ডুব
TOML তৈরি করেছেন টম প্রেস্টন-ওয়ের্নার, GitHub এর প্রতিষ্ঠাতাদের একজন, কনফিগারেশন ফাইলের জন্য JSON এর চেয়ে মানুষের পড়ার উপযুক্ত হতে, যদিও এটা অবিকল্পভাবে পার্স করার সক্ষমতা ধরে রাখে। এটি যতটা সহজ সম্ভব হওয়ার লক্ষ্য নিয়েছে, যা অনেক ডেভেলপমেন্ট প্রজেক্টের এথোসের সাথে সুন্দরভাবে মিলে যায় যেগুলি তাদের কোডবেজে সাদাসিধা এবং পাঠযোগ্যতার জন্য চেষ্টা করে।

Google Apps Script এর প্রেক্ষাপটে, TOML ব্যবহার করা কিছুটা ওভারহেড প্রবর্তন করে, যেহেতু সরাসরি সমর্থনের অভাব এবং ম্যানুয়ালি বা তৃতীয় পক্ষের লাইব্রেরিগুলির মাধ্যমে এটি পার্স করার প্রয়োজনীয়তা থাকে। ছোট প্রকল্পগুলির জন্য বা যেগুলি গুগলের ইকোসিস্টেমে গভীরভাবে একীভূত নয়, জেসন বা স্ক্রিপ্ট প্রোপার্টিজগুলিতে এমনকি সাধারণ কী-ভ্্র্�
