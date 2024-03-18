---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:55.784703-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML, একটি ডেটা সিরিয়ালাইজেশন ভাষা যা মানব-বান্ধব হবার জন্য ডিজাইন করা হয়েছে, প্রায়শই কনফিগারেশন ফাইলগুলি, ইন্টার-প্রসেস মেসেজিং এবং ডেটা স্টোরেজের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা YAML-এর পড়ার সহজতা এবং ব্যবহারের সুবিধা, বিশেষ করে জটিল গঠিত ডেটা নিয়ে কাজ করার সময়ে, এর উপর নির্ভর করে, যা এটিকে TypeScript-এ বিকশিত আবেদনগুলির জন্য একটি উত্কৃষ্ট পছন্দ করে তোলে।

## কিভাবে:

TypeScript-এ YAML এর সাথে কাজ করা সাধারণত YAML কন্টেন্টকে JavaScript অবজেক্টগুলিতে পার্সিং করা এবং সম্ভবত JavaScript অবজেক্টগুলিকে আবার YAML-এ রূপান্তর করার প্রয়োজন করে। এর জন্য একটি পার্সারের প্রয়োজন; একটি জনপ্রিয় পছন্দ হল `js-yaml`, যা TypeScript প্রকল্পগুলিতে সহজেই সমন্বিত করা যেতে পারে। 

### js-yaml ইনস্টল করা

প্রথমে, আপনার প্রকল্পে `js-yaml` যোগ করুন:

```bash
npm install js-yaml
```

### YAML কে JavaScript অবজেক্টে পার্সিং

ধরুন আপনার কাছে `config.yaml` নামে একটি YAML ফাইল আছে যার বিষয়বস্তু নিম্নরূপ:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

আপনি এই ফাইলটি একটি JavaScript অবজেক্টে পড়া এবং পার্সিং করতে পারেন এইভাবে:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// YAML ফাইলটি লোড এবং পার্স করা
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**স্যাম্পল আউটপুট:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScript অবজেক্ট কে YAML এ রূপান্তর

যদি আপনাকে অন্য দিকে যেতে হয় এবং একটি JavaScript অবজেক্টকে YAML স্ট্রিং-এ রূপান্তর করতে হয়, আপনি `js-yaml` ব্যবহার করতে পারেন এইভাবে:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**স্যাম্পল আউটপুট:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

এই স্নিপেটটি JavaScript অবজেক্টকে YAML স্ট্রিং-এ রূপান্তর করে এবং এটি আউটপুট করে। অনুশীলনে, আপনি এটি একটি ফাইলে লেখার পিছনে বা আপনার আবেদনের অন্যান্য অংশে ব্যবহার করতে পারেন।
