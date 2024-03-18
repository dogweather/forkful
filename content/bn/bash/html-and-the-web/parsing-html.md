---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:07.721152-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

HTML পার্সিং মানে HTML ফাইলের গঠন এবং বিষয়বস্তু পরীক্ষা করে তথ্য আহরণ করা। প্রোগ্রামাররা এটি করে ডেটা অ্যাক্সেস, বিষয়বস্তু পরিচালনা, বা ওয়েবসাইট থেকে তথ্য সংগ্রহের জন্য।

## কিভাবে:

Bash HTML পার্সিং-এর জন্য প্রাথমিক পছন্দ নয়, তবে `grep`, `awk`, `sed`, অথবা বাহ্যিক টুলস যেমন `lynx` এর মাধ্যমে এটি করা সম্ভব। শক্তিশালীতা এর জন্য, আমরা `libxml2` প্যাকেজ থেকে `xmllint` ব্যবহার করব।

```bash
# xmllint ইন্সটল করুন যদি প্রয়োজন হয়
sudo apt-get install libxml2-utils

# নমুনা HTML
cat > sample.html <<EOF
<html>
<head>
  <title>উদাহরণ পেজ</title>
</head>
<body>
  <h1>হ্যালো, Bash!</h1>
  <p id="myPara">Bash আমাকে পড়তে পারে।</p>
</body>
</html>
EOF

# শিরোনাম পার্স করুন
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "শিরোনাম হল: $title"

# ID দ্বারা অনুচ্ছেদ এক্সট্র্যাক্ট করুন
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "অনুচ্ছেদ বিষয়বস্তু হল: $para"
```

আউটপুট:
```
শিরোনাম হল: উদাহরণ পেজ
অনুচ্ছেদ বিষয়বস্তু হল: Bash আমাকে পড়তে পারে।
```

## গভীরে দেখা

পূর্বে, প্রোগ্রামাররা HTML স্ক্যান করার জন্য `grep` এর মতো regex-ভিত্তিক টুলস ব্যবহার করত, যা ছিল বিশৃঙ্খল। HTML নিয়মিত নয়—এটি প্রসঙ্গমূলক। ঐতিহ্যবাহী টুলস এই বিষয়টি উপেক্ষা করে এবং তা ভুলের সম্ভাবনা বাড়ায়।

বিকল্প? অনেক আছে। পাইথন দিয়ে বিউটিফুল স্যুপ, PHP দিয়ে DOMDocument, জাভাস্ক্রিপ্টে DOM পার্সার— এমন ভাষা যাদের লাইব্রেরি HTML এর গঠন বোঝার জন্য ডিজাইন করা।

Bash স্ক্রিপ্টে `xmllint` ব্যবহার করা সাধারণ কাজের জন্য দৃঢ়। এটি XML বোঝে এবং তত্ত্বাবধানে, XHTML। নিয়মিত HTML অনিশ্চিত হতে পারে, এটা সর্বদা XML এর কঠোর নিয়ম অনুসরণ করে না। `xmllint` HTML কে XML মডেলে পরিণত করে যা ভালভাবে গঠিত HTML এর জন্য ভালো কাজ করে, কিন্তু অগোছালো জিনিস নিয়ে সমস্যা তৈরি করতে পারে।

## দেখুন

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): HTML DOM সম্পর্কে জটিলতা দূর করে।
- [MDN ওয়েব ডক - XML পার্সিং এবং সিরিয়ালাইজেশন](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): XHTML এ প্রযোজ্য XML পার্সিং নীতি।
- [বিউটিফুল স্যুপ ডকুমেন্টেশন](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): HTML পার্সিং এর জন্য একটি পাইথন লাইব্রেরি।
- [libxml2 ডকুমেন্টেশন](http://xmlsoft.org/): `xmllint` এবং সংশ্লিষ্ট XML টুলস সম্পর্কে তথ্য।
