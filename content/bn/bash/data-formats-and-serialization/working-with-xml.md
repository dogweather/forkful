---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:33.792593-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AC\u09BE\u09B6\u09C7 XML \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  \u0964 \u099F\u09C1\u09B2\u09B8? xmllint \u098F\u09AC\u0982 xmlstarlet\u0964 XML\
  \ \u098F\u09B2\u09BF\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09B2\
  \u09C1\u09AA \u0995\u09B0\u09BE? \u0985\u09AC\u09B6\u09CD\u09AF\u0987\u0964 \u09A8\
  \u09AE\u09C1\u09A8\u09BE \u0986\u0989\u099F\u09AA\u09C1\u099F \u09B8\u09B9 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3."
lastmod: '2024-03-17T18:47:44.252487-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09BE\u09B6\u09C7 XML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\
  \u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964 \u099F\u09C1\u09B2\u09B8."
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
বাশে XML পার্স করার উপায় এখানে দেওয়া হল। টুলস? xmllint এবং xmlstarlet। XML এলিমেন্টগুলি লুপ করা? অবশ্যই। নমুনা আউটপুট সহ উদাহরণ:

```bash
# ধরা হয়েছে xmlstarlet ইনস্টল করা আছে
# ইনস্টল করতে: apt-get install xmlstarlet

# XML কন্টেন্ট পার্সিং
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# xmlstarlet দিয়ে নামগুলো নির্যাতন
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# আউটপুট হওয়া উচিত:
# Apple
# Banana
```

## গভীর ডুব
৯০ এর দশকে, XML SGML এর একটি সহজ বিকল্প হিসেবে উপস্থিত হয়েছিল, কিন্তু HTML এর চেয়ে বেশি গঠনমূলক। এখন, এর সঙ্গী আছে – JSON, YAML, যেমন। কিন্তু XML এখনও টিকে আছে, বিশেষ করে কনফিগ এবং SOAP-ভিত্তিক ওয়েব সেবাগুলিতে।

টুলের দিক থেকে, xmllint XML ভ্যালিডেশন, xpath কোয়েরিগুলির জন্য আরামদায়ক। xmlstarlet হল XML কান্ডকারখানার জন্য সুইস-আর্মি ছুরি – প্রশ্ন, সম্পাদনা, ভ্যালিডেশন, ট্রান্সফর্ম। ব্যাশ স্ক্রিপ্টগুলিতে, তারা XML কাজের জন্য সুপারহিরো।

অধীনস্থভাবে, xmllint libxml2 ব্যবহার করে – এক্সএমএল সি পার্সার। এটি দ্রুত, কিন্তু ত্রুটি বার্তাগুলো? রহস্যময়। এবং xmlstarlet? পুনরাবৃত্তিমূলক টেমপ্লেট এবং EXSLT সাপোর্ট। মনে রাখবেন, কিন্তু শক্তিশালী।

## আরও দেখুন
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 এবং xmllint বিষয়বস্তু।
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): বাস্তব সমস্যা এবং সমাধান।
- [W3Schools XML টিউটোরিয়াল](https://www.w3schools.com/xml/): XML এর মৌলিক জ্ঞান।
