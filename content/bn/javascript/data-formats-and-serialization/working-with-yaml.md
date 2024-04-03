---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:08.482300-06:00
description: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\
  \u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\u09BF\
  \ \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09A1\
  \u09BE\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\
  \u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u098F\u09B0\
  \ \u09B8\u09BE\u09AE\u09BE\u09A8\u09CD\u09AF\u09A4\u09BE \u0993 \u09AA\u09BE\u09A0\
  \u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09B0 \u0995\u09BE\u09B0\u09A3\
  \u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:44.473770-06:00'
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\u09CD\
  \u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\u09BF \u09AE\
  \u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09A1\u09BE\
  \u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\
  \u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u098F\u09B0 \u09B8\
  \u09BE\u09AE\u09BE\u09A8\u09CD\u09AF\u09A4\u09BE \u0993 \u09AA\u09BE\u09A0\u09AF\
  \u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 JSON \u0985\u09A5\u09AC\u09BE XML \u098F\
  \u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE\u09AF\u09BC \u0995\u09A8\u09AB\u09BF\u0997\
  \u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u0993 \u09AD\u09BE\u09B7\
  \u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE \u0986\u09A6\
  \u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8\u0964."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
