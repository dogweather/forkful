---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:30:58.162599-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

TOML, যা Tom's Obvious, Minimal Language এর জন্য দাঁড়ায়, একটি কনফিগারেশন ফাইল ফর্ম্যাট যা এর স্পষ্ট সেমান্টিক্সের কারণে পড়তে সহজ। অ্যাপ্লিকেশনগুলিতে কনফিগারেশন ফাইলের জন্য প্রোগ্রামাররা প্রায়ই এটি ব্যবহার করে, কারণ এটি সরল এবং মানুষের পড়ার উপযুক্ত, যা বিভিন্ন পরিবেশে অ্যাপ্লিকেশন সেটিংস এবং কনফিগারেশনগুলির ব্যবস্থাপনা নির্বিঘ্নে করে তোলে।

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
