---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:32.980940-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u098F\u0995\u099F\u09BF TOML \u09AA\u09BE\
  \u09B0\u09CD\u09B8\u09BE\u09B0\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u09B9\u09AC\u09C7\u0964 `@iarna/toml` \u098F\u0995\u099F\u09BF \u099C\u09A8\
  \u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\u09A6\u0964 npm \u09A6\
  \u09BF\u09AF\u09BC\u09C7 \u098F\u099F\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u0995\u09B0\u09C1\u09A8: `npm install @iarna/toml --save`\u0964\u2026"
lastmod: '2024-03-17T18:47:43.789254-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF TOML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\u09B0\u09C7\u09B0 \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AC\u09C7\u0964 `@iarna/toml`\
  \ \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\
  \u099B\u09A8\u09CD\u09A6\u0964 npm \u09A6\u09BF\u09AF\u09BC\u09C7 \u098F\u099F\u09BF\
  \ \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
প্রথমে, আপনার একটি TOML পার্সারের প্রয়োজন হবে। `@iarna/toml` একটি জনপ্রিয় পছন্দ। npm দিয়ে এটি ইনস্টল করুন: `npm install @iarna/toml --save`। এখানে কিভাবে একটি TOML ফাইল পড়তে এবং এটিকে একটি জাভাস্ক্রিপ্ট অবজেক্টে পার্স করতে হয়:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
যদি `config.toml` এ থাকে:
```
[server]
port = 8080
```
আউটপুট হবে:
```
{ server: { port: 8080 } }
```
এবং, একটি TOML ফাইলে লিখন সমানভাবে সোজা:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
এই কোড চালালে অবজেক্টটি TOML ফরম্যাটে `config.toml` এ লেখা হয়।

## গভীর ডুব
TOML টম প্রেস্টন-ওয়ের্নার দ্বারা তৈরি করা হয়েছিল, যিনি GitHub এর সহ-প্রতিষ্ঠাতা, ২০১৩ সালের দিকে INI বা YAML এর মতো অন্যান্য ফরম্যাটের সীমাবদ্ধতার প্রতি তাঁর উপলব্ধির প্রতিক্রিয়া হিসেবে। এটি অস্পষ্টতা মুক্ত এবং ডেটা কাঠামোগত পার্স করা সহজ হিসেবে ডিজাইন করা হয়েছে, সুতরাং, কনফিগারেশন ফাইলের জন্য একটি প্রিয়। JSON মতো বিকল্পগুলি মন্তব্যের অভাব রয়েছে, যেখানে YAML আরও জটিল। TOML এর সাধারণ ভাব এবং জটিল ডেটা জেরার্কিকে স্পষ্টভাবে প্রতিনিধিত্ব করার ক্ষমতা এটিকে বিশেষ করে তোলে।

অন্তর্নিহিতভাবে, যখন আপনি TypeScript এ TOML পার্স করেন, আপনি মৌখিক ডেটাকে এমন একটি কাঠামোগত ফরম্যাটে পরিবর্তন করছেন যে ভাষা তা নিয়ন্ত্রণ করতে পারে। এটি লেক্সিং (কাঁচা লেখা থেকে টোকেনে পরিবর্তন) এবং পার্সিং (একটি অভ্যন্তরীণ ডেটা কাঠামো তৈরি) জড়িত; `@iarna/toml` উভয়ই নির্বিঘ্নে সামলায়। ইমোজি সমর্থন একটি মজার বিষয়, যা TOML এর ব্যবহারকারী-কেন্দ্রিক প্রবণতাকে দেখায়।

## দেখুন
- TOML অফিসিয়াল স্পেক: https://toml.io/en/
- `@iarna/toml` প্যাকেজ: https://www.npmjs.com/package/@iarna/toml
- TOML, YAML, এবং JSON এর মধ্যে তুলনা: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
