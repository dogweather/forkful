---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:27.650995-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
HTML পার্সিং মানে HTML ডকুমেন্ট থেকে ডেটা প্রত্যাহার করা। প্রোগ্রামাররা ওয়েব কন্টেন্টের সাথে ইন্টার‍্যাক্ট করতে, ডেটা প্রত্যাহার অটোমেট করতে বা ওয়েব স্ক্র্যাপিং উদ্দেশ্যে এটি করে থাকে।

## কীভাবে:
চলুন JavaScript-এ `DOMParser` API ব্যবহার করে HTML পার্স করি।

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // আউটপুট: Hello, world!
```

এখন, চলুন আরও নির্দিষ্ট কিছু নেই, যেমন একটি ক্লাসের সাথে একটি এলিমেন্ট:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // আউটপুট: Hello, again!
```

## গভীরে প্রবেশ
HTML পার্সিং ওয়েব যতদিনের ততদিনের বিষয়। প্রথমদিকে, এটি একটি ব্রাউজারের বিষয় ছিল—ব্রাউজারগুলি ওয়েব পৃষ্ঠাগুলি প্রদর্শনের জন্য HTML পার্স করত। সময়ের সাথে সাথে, প্রোগ্রামাররা এই প্রক্রিয়ায় নিজেদের প্রবেশ করাতে চেয়েছিল, যা `DOMParser` এর মতো API লিডিং করে।

বিকল্প? অবশ্যই আছে। আমাদের কাছে `jQuery` এবং `BeautifulSoup` এর মতো টুলস আছে পাইথনের জন্য। কিন্তু JavaScript-এর নেটিভ `DOMParser` হল দ্রুত এবং অন্তর্বতী, অতিরিক্ত লাইব্রেরিগুলির প্রয়োজন নেই।

বাস্তবায়নের দিক থেকে, যখন আপনি `DOMParser` দিয়ে HTML পার্স করেন, এটি একটি `Document` অবজেক্ট তৈরি করে। আপনার HTML-এর একটি হায়ারার্কিক্যাল মডেল হিসেবে এটিকে ভাবুন। একবার আপনার কাছে এটি থাকলে, আপনি এটি ন্যাভিগেট এবং ম্যানিপুলেট করতে পারেন যেমনটি আপনি একটি সাধারণ ওয়েব পৃষ্ঠার DOM এর সাথে করে থাকেন।

এখানে ব্যাপারটা হল—পার্সিং ভুল ফর্ম্যাট করা HTML-এ সমস্যা সৃষ্টি করতে পারে। ব্রাউজারগুলি ক্ষমাশীল হয়, কিন্তু `DOMParser` হয়ত নয়। তাই, জটিল কাজ বা অগোছালো HTML-এর জন্য, থার্ড-পার্টি লাইব্রেরিগুলি ভাল পরিষ্কার কাজ করতে পারে।

## দেখুন অতঃপর
- `DOMParser` API সম্পর্কে MDN ওয়েব ডকস: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery-এর পার্সিং ক্ষমতা: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- সার্ভারের জন্য jQuery-এর মূল বাস্তবায়নের দ্রুত, নমনীয় ও পাতলা প্রকাশ: [Cheerio.js](https://cheerio.js.org/)
- নন-জেএস পার্সিং: পাইথনের BeautifulSoup লাইব্রেরি: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
