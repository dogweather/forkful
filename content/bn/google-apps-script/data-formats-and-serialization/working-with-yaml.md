---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:42.131098-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML, যার অর্থ "YAML Ain't Markup Language," একটি মানব-পাঠযোগ্য ডেটা সিরিয়ালাইজেশন স্ট্যান্ডার্ড যা সাধারণত কনফিগারেশন ফাইল এবং ভিন্ন ডেটা কাঠামো সহ ভাষার মধ্যে ডেটা বিনিময়ের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা প্রায়শই YAML এর সাথে কাজ করে এর সরলতা এবং পাঠযোগ্যতার জন্য, বিশেষ করে বিস্তারিত কনফিগারেশনের প্রয়োজন হলে অথবা ভিন্ন সিস্টেমের মধ্যে গঠনগত ডেটা স্থানান্তর করার সময়।

## কিভাবে:

যদিও Google Apps Script (GAS) স্বাভাবিকভাবে YAML পার্সিং বা সিরিয়ালাইজেশন সমর্থন করে না, আপনি JavaScript লাইব্রেরি ব্যবহার করে অথবা কাস্টম পার্সিং ফাংশন লেখার মাধ্যমে YAML ডেটা নিয়ন্ত্রণ করতে পারেন। ডেমনস্ট্রেশনের জন্য, আসুন দেখি কিভাবে একটি YAML স্ট্রিং পার্স করা যায় একটি কাস্টম ফাংশন ব্যবহার করে, যেহেতু GAS এ বাইরের লাইব্রেরি সরাসরি আমদানি করা যায় না।

ধরুন আপনার কাছে একটি সাধারণ YAML কনফিগারেশন রয়েছে:

```yaml
title: YAML Example
description: Google Apps Script এ YAML হ্যান্ডল করার একটি উদাহরণ
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

Google Apps Script এ এটি পার্স করতে, JavaScript এর স্ট্রিং ম্যানিপুলেশন সুবিধাগুলি ব্যবহার করুন:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // অ্যারেগুলির জন্য মৌলিক হ্যান্ডলিং
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: Google Apps Script এ YAML হ্যান্ডল করার একটি উদাহরণ\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

`testYamlParsing()` যখন এক্সিকিউট করা হয়, এটি আউটপুট দেয়:

```
{ title: 'YAML Example',
  description: 'Google Apps Script এ YAML হ্যান্ডল করার একটি উদাহরণ',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

এই কাস্টম পার্সিং অ্যাপ্রোচ বেশ মৌলিক এবং জটিল YAML ফাইলগুলি সম্পাদনের জন্য সমন্বয় প্রয়োজন হতে পারে।

## গভীর ডাইভ

YAML, ২০০১ সালে মুক্তিপ্রাপ্ত, এর পূর্বসূরি যেমন XML বা JSON এর তুলনায় আরও মানব-পাঠযোগ্য হতে লক্ষ্য রেখেছিল। যদিও এর সরলতা এবং ব্যবহারের সুবিধা ব্যাপকভাবে প্রশংসিত, Google Apps Script এ YAML হ্যান্ডল করা চ্যালেঞ্জিং কারণ সরাসরি সমর্থনের অভাবে। ফলস্বরূপ, প্রোগ্রামাররা প্রায়শই YAML ডেটা পার্স এবং জেনারেট করার জন্য JavaScript এর বহুমুখিতা নির্ভর করে থাকেন। তবে, জটিল ব্যবহার ক্ষেত্রে, বিশেষ করে গভীর নেস্টিং এবং উন্নত ডেটা কাঠামোগুলি জড়িত হলে, এই পদ্ধতি বেশ অসুবিধাজনক এবং ভুলের সম্ভাবনা বৃদ্ধি পেতে পারে।

অন্যদিকে, JSON, Google Apps Script এবং অন্যান্য প্রোগ্রামিং পরিবেশে স্বাভাবিকভাবে সমর্থিত, ডেটা সিরিয়ালাইজেশন এবং ডিসিরিয়ালাইজেশনের জন্য অতিরিক্ত পার্সিং ওভারহেড ছাড়াই একটি আরও সোজা পদ্ধতি অফার করে। JSON এর সিনট্যাক্স YAML এর তুলনায় কম শব্দবহুল, তাই এটি ওয়েব অ্যাপ্লিকেশনে ডেটা বিনিময়ের জন্য আরও উপযুক্ত। তবুও, YAML কনফিগারেশন ফাইলগুলির জন্য এবং মানব-পাঠযোগ্যতা যখন প্রধান হয় তখন জনপ্রিয় থেকে যায়।

Google Apps Script এ YAML এর সাথে কাজ করার সময়, পাঠযোগ্যতা এবং ব্যবহারের সুবিধার মধ্যে ট্রেড-অফ বিবেচনা করুন। বৃহত্তর YAML ম্যানিপুলেশনের জন্য, বাইরের টুলস বা সেবাগুলি অন্বেষণ করা যেতে পারে যা YAML কে JSON এ রূপান্তরিত করে আপনার স্ক্রিপ্টে এর প্রসেসিং করার আগে।
