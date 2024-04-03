---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:59.876884-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Node.js \u09AA\u09B0\u09BF\u09AC\
  \u09C7\u09B6\u09C7, \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\
  \ \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BF\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\
  \ `fs` (File System) \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\
  \u0987 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u099F\u09BF \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u0985\u09CD\u09AF\u09BE\u09B8\u09BF\u0999\u09CD\
  \u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8\u09B2\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.471800-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7, \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0986\u09AA\u09A8\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\
  \u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 `fs` (File System) \u09AE\u09A1\u09BF\u0989\
  \u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0987 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \u099F\u09BF \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u0985\u09CD\
  \u09AF\u09BE\u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8\u09B2\
  \u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u0989\
  \u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09AF\u09BC."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
Node.js পরিবেশে, টেক্সট ফাইল লেখার জন্য আপনি অন্তর্নির্মিত `fs` (File System) মডিউল ব্যবহার করতে পারেন। এই উদাহরণটি একটি ফাইলে অ্যাসিঙ্ক্রোনাসলি টেক্সট লেখার উপায় দেখায়:

```javascript
const fs = require('fs');

const ডেটা = 'হ্যালো, ওয়ার্ল্ড! এটি একটি ফাইলে লেখা হবে এমন টেক্সট।';

fs.writeFile('example.txt', ডেটা, (err) => {
  if (err) {
    throw err;
  }
  console.log('ফাইল লেখা হয়েছে।');
});
```

নমুনা আউটপুট:
```
ফাইল লেখা হয়েছে।
```

সিঙ্ক্রোনাস ফাইল লেখার জন্য, `writeFileSync` ব্যবহার করুন:
```javascript
try {
  fs.writeFileSync('example.txt', ডেটা);
  console.log('ফাইল লেখা হয়েছে।');
} catch (error) {
  console.error('ফাইল লেখায় ত্রুটি:', error);
}
```

আধুনিক ওয়েব ব্রাউজারগুলিতে, ফাইল সিস্টেম অ্যাক্সেস API ফাইল পড়া এবং লেখার ক্ষমতা প্রদান করে। তবে, এর ব্যবহার ইউজারের অনুমতির উপর নির্ভরশীল। এখানে একটি ফাইল তৈরি এবং তাতে লেখার উপায় দেওয়া হল:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('হ্যালো, ওয়ার্ল্ড! এটি ব্রাউজারের টেক্সট ফাইল লেখা।');
  await writable.close();
}
```

আরও জটিল পরিস্থিতি অথবা বড় ফাইলের সাথে কাজ করার সময়, আপনি ব্রাউজারের জন্য ‘FileSaver.js’ এর মতো তৃতীয় পক্ষের লাইব্রেরী বেছে নিতে পারেন:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["হ্যালো, ওয়ার্ল্ড! এটি FileSaver.js থেকে টেক্সট।"], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

মনে রাখবেন, ক্লায়েন্ট-সাইডে (ব্রাউজারগুলিতে) ফাইল লেখা নিরাপত্তা উদ্বেগের কারণে সীমাবদ্ধ, এবং ইউজারের স্থানীয় ডিস্কে সেভ করার যেকোনো অপারেশন সাধারণত তাদের স্পষ্ট অনুমতি প্রয়োজন হবে।
