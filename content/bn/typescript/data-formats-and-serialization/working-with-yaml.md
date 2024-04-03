---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:55.784703-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F YAML \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3\u09A4 YAML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u0995\
  \u09C7 JavaScript \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09B0\u09BE \u098F\
  \u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\u09AC\u09A4 JavaScript \u0985\u09AC\u099C\u09C7\
  \u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0986\u09AC\u09BE\u09B0\
  \ YAML-\u098F\u2026"
lastmod: '2024-03-17T18:47:43.786221-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 YAML \u0995\
  \u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u0995\u09C7 JavaScript \u0985\u09AC\u099C\
  \u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\
  \u09AC\u09A4 JavaScript \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 \u0986\u09AC\u09BE\u09B0 YAML-\u098F \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\u09B0\u09C7\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8; \u098F\u0995\u099F\u09BF \u099C\
  \u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\u09A6 \u09B9\
  \u09B2 `js-yaml`, \u09AF\u09BE TypeScript \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B8\u09B9\u099C\u09C7\u0987 \u09B8\u09AE\
  \u09A8\u09CD\u09AC\u09BF\u09A4 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964\n\n#."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
