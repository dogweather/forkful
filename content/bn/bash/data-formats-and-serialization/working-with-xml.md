---
title:                "XML নিয়ে কাজ করা"
date:                  2024-03-17T18:31:33.792593-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?
XML এর সাথে কাজ করা মানে পার্সিং, ডেটা নির্যাতন এবং এক্সটেন্সিবল মার্কআপ ল্যাঙ্গুয়েজ ফর্ম্যাটে ডেটা পরিচালনা করা। প্রোগ্রামাররা XML নিয়ে জর্জরিত হন কারণ এটি কনফিগ, এপিআই, এবং আরো অনেক কিছুর জন্য একটি প্রচলিত ডেটা ইন্টারচেঞ্জ ফর্ম্যাট।

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
