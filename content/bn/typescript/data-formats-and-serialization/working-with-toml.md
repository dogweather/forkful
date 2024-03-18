---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:31:32.980940-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
TOML, যা Tom's Obvious, Minimal Language এর সংক্ষেপ, একটি ডেটা সিরিয়ালাইজেশন ফরম্যাট যা JSON বা YAML এর মতোই। প্রোগ্রামাররা এটি এর মানব পঠনযোগ্যতা এবং ডেটা টাইপগুলিতে সরাসরি ম্যাপিং এর জন্য ব্যবহার করে, যা এটিকে কনফিগ ফাইল এবং ডেটা আদান-প্রদানের জন্য একটি যাওয়া-আসা করা বিকল্প করে তোলে।

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
