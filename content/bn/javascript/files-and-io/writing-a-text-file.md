---
title:                "একটি টেক্সট ফাইল লিখা"
date:                  2024-03-17T18:39:59.876884-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?
JavaScript-এ টেক্সট ফাইল লেখা প্রায়শই লগিং, ইউজার ইনপুট এক্সপোর্ট করা, অথবা কনফিগুরেশন উদ্দেশ্যে ডেটা তৈরি করা এবং সেভ করার সাথে জড়িত। অ্যাপ্লিকেশনের প্রক্রিয়া জীবনকালের বাইরে ডেটা সংরক্ষণ প্রয়োজন হলে এই কার্যকারিতা খুবই জরুরী, যা তথ্য স্টোর করা এবং পরে পুনরুদ্ধার অথবা শেয়ার করার উপায় প্রদান করে।

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
