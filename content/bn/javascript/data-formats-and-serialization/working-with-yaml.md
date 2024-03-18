---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:08.482300-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML, "YAML Ain't Markup Language" এর সংক্ষিপ্ত রূপ, একটি মানব-পাঠযোগ্য ডাটা সিরিয়ালাইজেশন ফরম্যাট। এর সামান্যতা ও পাঠযোগ্যতা এর কারণে প্রোগ্রামাররা প্রায়ই JSON অথবা XML এর তুলনায় কনফিগারেশন ফাইল ও ভাষার মধ্যে ডাটা আদান-প্রদানে এটি ব্যবহার করে থাকেন।

## কিভাবে:

JavaScript-এ, YAML এর সাথে কাজ করা সাধারণত তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করে হয় যেহেতু ভাষাটিতে YAML পার্সারের জন্য নির্মিত কোনো সুবিধা নেই। এই উদ্দেশ্যে সবচেয়ে জনপ্রিয় লাইব্রেরিগুলির একটি হল `js-yaml`। আপনি `js-yaml` ব্যবহার করে YAML কে JavaScript অবজেক্টে পার্স করতে এবং উল্টাটাও করতে পারবেন।

প্রথমত, আপনাকে `js-yaml` ইন্সটল করতে হবে:

```bash
npm install js-yaml
```

এরপর, আপনি আপনার প্রজেক্টগুলিতে এটি ব্যবহার করতে পারবেন। এখানে দেখানো হল কিভাবে একটি YAML ফাইল লোড করে তা JavaScript অবজেক্টে পার্স করা যায়:

```javascript
// js-yaml মডিউল রিকোয়ার করুন
const yaml = require('js-yaml');
const fs   = require('fs');

// ফাইল থেকে YAML লোড করুন
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

যদি আপনার `config.yaml` ফাইলটি এইরকম হয়:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

তাহলে আউটপুট হবে:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

উল্টো, একটি JavaScript অবজেক্ট থেকে YAML স্ট্রিং-এ রূপান্তর করতে:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

এই কোডটি উৎপন্ন করবে:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

`js-yaml` ব্যবহার করে, আপনি সহজেই আপনার JavaScript প্রজেক্টগুলিতে YAML পার্সিং এবং সিরিয়ালাইজেশন একীভূত করতে পারবেন, যা ডাটা ইন্টারচেঞ্জেবিলিটি এবং কনফিগারেশন ম্যানেজমেন্টকে উন্নত করে।
