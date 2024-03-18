---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:55.013544-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Google Apps Script এ HTML পার্সিং মূলত ওয়েব পেজ বা ওয়েব ভিত্তিক ডাটা সোর্স থেকে ডাটা বের করে আনার প্রক্রিয়া, যা ওয়েব পেজের সাথে মিথস্ক্রিয়া করার ক্ষেত্রে বিশেষভাবে উপযোগী। প্রোগ্রামারগণ এটি ডাটা সংগ্রহ, ওয়েব কন্টেন্ট পরিবর্তন, অথবা Google Apps যেমন Sheets এবং Docs এর সাথে ওয়েব ফাংশনালিটিসকে ইন্টিগ্রেট করার জন্য করে থাকে।

## কিভাবে:
Google Apps Script এ HTML পার্স করার জন্য কোন নির্মিত পদ্ধতি নেই। তবে, আপনি `UrlFetchApp` সার্ভিসের সাহায্যে HTML কন্টেন্ট আনতে এবং তারপরে JavaScript মেথড বা regex (নিয়মিত প্রকাশ) ব্যবহার করে পার্স করতে পারেন। নীচে একটি ওয়েব পেজ থেকে টাইটেল ট্যাগ ফেচ এবং পার্স করার একটি মৌলিক উদাহরণ দেওয়া হলো।

```javascript
function parseHTMLTitle(url) {
  // ওয়েব পেজের HTML কন্টেন্ট আনুন
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // <title> ট্যাগের কন্টেন্ট খুঁজে পেতে একটি সিম্পল regex ব্যবহার করুন
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // চেক করুন একটি টাইটেল পাওয়া গেছে কিনা এবং তা ফেরত দিন
  if (match && match.length > 1) {
    return match[1];
  }

  return 'No title found';
}

// উদাহরণ ব্যবহার
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // ওয়েব পেজের টাইটেল আউটপুট করে
```

আরও জটিল HTML পার্সিং এর জন্য, আপনি `XmlService` ব্যবহার করে HTML কে XML হিসেবে পার্স করতে পারেন। মনে রাখবেন, এটি আবশ্যক যে HTML টি ভাল গঠিত XML হতে হবে, যা সবসময় ঘটে না:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // এখান থেকে, XmlService পদ্ধতি ব্যবহার করে XML ট্রি নেভিগেট করুন
    // উদাহরণস্বরূপ, একটি নির্দিষ্ট এলিমেন্ট বা এট্রিবিউট খুঁজে পেতে
  } catch(e) {
    Logger.log('HTML পার্স করতে গিয়ে সমস্যা: ' + e.toString());
  }
}
```

## গভীর ডুব:
ইতিহাসে, Google Apps Script এর মতো পরিবেশে HTML পার্সিং চ্যালেঞ্জিং হয়েছে, কারণ এখানে Document Object Model (DOM) অথবা পার্সিং-এর জন্য নিবেদিত লাইব্রেরি গুলির অভাব রয়েছে, যা অন্যান্য প্রোগ্রামিং প্রসঙ্গে সাধারণ। যেমন, ব্রাউজারে JavaScript-এ DOM সহজলভ্য, এবং Node.js পরিবেশে `cheerio` বা `jsdom` এর মতো NPM প্যাকেজ গুলি HTML পার্সিং-এর জন্য পাওয়া যায়।

Google Apps Script-এর পদ্ধতি মূলত `UrlFetchApp` ব্যবহার করে ওয়েব অনুরোধ এবং তারপরে রেসপন্স ডেটার সাথে regex বা XML পার্সিং পদ্ধতি ব্যবহার করা। যদিও regex সহজ পার্সিং কাজের জন্য উপকারী হতে পারে, জটিল HTML এর জন্য এটি সাধারণত উপদেশ করা হয় না, কারণ এতে ভুল এবং কোডের ভঙ্গুর সম্ভাবনা থাকে। XML পার্সিং এর মাধ্যমে `XmlService` একটি আরও structured পদ্ধতি অফার করে but এটি well-formed HTML/XML এর প্রয়োজন, যা যেকোনো ওয়েব পেজের সাথে DEAL করার সময় একটি বাধা হতে পারে।

জটিল পার্সিং প্রয়োজনে অথবা খারাপভাবে গঠিত HTML এর সাথে DEAL করার জন্য, একটি বিকল্প কৌশল হতে পারে Google Apps Script-এর বাইরে একটি ওয়েব সার্ভিস ব্যবহার করা। এই সার্ভিসটি HTML কন্টেন্ট প্রক্রিয়া করতে পারে, সম্ভবত একটি আরও রোবাস্ট পার্সিং টেকনিক বা লাইব্রেরি ব্যবহার করে, এবং তারপরে প্রক্রিয়াকৃত ডেটা ফিরিয়ে দিতে পারে যা Google Apps Script দ্বারা সহজে গ্রহণযোগ্য ফর্মে। তবে, এই পদ্ধতিটি নেটওয়ার্ক বিলম্ব এবং একটি অতিরিক্ত ওয়েব সার্ভিস পরিচালনার জটিলতা চালু করে।

এই চ্যালেঞ্জগুলি সত্ত্বেও, Google Apps Script এর মধ্যে HTML পার্সিং একটি শক্তিশালী টুল remains, বিশেষ করে অন্যান্য Google সার্ভিস এবং API এর সাথে সমন্বিত হলে, যা প্রোডাক্টিভিটি এবং ডাটা প্রসেসিং ক্ষমতা উল্লেখযোগ্যভাবে বাড়াতে পারে।
