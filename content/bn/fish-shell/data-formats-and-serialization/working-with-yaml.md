---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:21.658496-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish Shell \u098F\u09B0 YAML \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\
  \u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\
  \u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\
  \u09BF `yq` (\u098F\u0995\u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u098F\u09AC\
  \u0982 \u09AA\u09CB\u09B0\u09CD\u099F\u09C7\u09AC\u09B2 \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1-\u09B2\u09BE\u0987\u09A8 YAML \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\u09B0\
  ) \u098F\u09B0 \u09AE\u09A4 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\
  \u09B7\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.518702-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u098F\u09B0 YAML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\
  \u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\
  \u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `yq` (\u098F\u0995\u099F\u09BF\
  \ \u09B9\u09BE\u09B2\u0995\u09BE \u098F\u09AC\u0982 \u09AA\u09CB\u09B0\u09CD\u099F\
  \u09C7\u09AC\u09B2 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8\
  \ YAML \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\u09B0) \u098F\u09B0 \u09AE\u09A4 \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u099F\u09C1\
  \u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 YAML \u09A1\
  \u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\n\n**yq \u0987\u09A8\u09B8\u09CD\
  \u099F\u09B2\u09C7\u09B6\u09A8 (\u09AF\u09A6\u09BF \u0986\u0997\u09C7 \u09A5\u09C7\
  \u0995\u09C7 \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u09A8\u09BE \u0995\u09B0\u09BE\
  \ \u09B9\u09DF\u09C7 \u09A5\u09BE\u0995\u09C7):**."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
Fish Shell এর YAML পার্স করার জন্য কোনো বিল্ট-ইন সাপোর্ট নেই, তবে আপনি `yq` (একটি হালকা এবং পোর্টেবল কমান্ড-লাইন YAML প্রসেসর) এর মত তৃতীয় পক্ষের টুল ব্যবহার করে YAML ডেটা নিয়ে কাজ করতে পারেন।

**yq ইনস্টলেশন (যদি আগে থেকে ইনস্টল না করা হয়ে থাকে):**
```fish
sudo apt-get install yq
```

**একটি YAML ফাইল থেকে মান পড়া:**
ধরুন আপনার কাছে `config.yaml` নামে একটি YAML ফাইল আছে যার ভিতরে নিম্নলিখিত সামগ্রী রয়েছে:
```yaml
database:
  host: localhost
  port: 3306
```

ডাটাবেস হোস্ট পড়তে, আপনি ব্যবহার করবেন:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**নমুনা আউটপুট:**
```
localhost
```

**একটি YAML ফাইলে মান আপডেট করা:**
`port` কে `5432` এ আপডেট করতে, ব্যবহার করুন:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**আপডেট যাচাই করা:**
```fish
yq e '.database.port' config.yaml
```
**নমুনা আউটপুট:**
```
5432
```

**নতুন একটি YAML ফাইল তৈরি:**
পূর্বনির্ধারিত সামগ্রী সহ একটি নতুন `new_config.yaml` তৈরি করার জন্য:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
এটি `yq` ব্যবহার করে একটি স্ট্রিংকে প্রসেস করে এবং পুটি-প্রিন্ট (-P ফ্ল্যাগ) করে একটি নতুন YAML ফাইলে রূপান্তর করে।

**জটিল গঠনপ্রণালী পার্স করা:**
যদি আপনার কাছে আরও জটিল একটি YAML ফাইল থাকে ও আপনাকে নেস্টেড অ্যারে বা অবজেক্ট আনতে হয়, আপনি পারেন:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**নমুনা আউটপুট:**
```
server1
server2
```
`yq` ব্যবহার করে, Fish Shell আপনাকে YAML ডকুমেন্টগুলির মধ্য দিয়ে নেভিগেট করে ও বিভিন্ন অটোমেশন ও কনফিগারেশন কাজে সেগুলি ম্যানিপুলেট করার জন্য সোজাসাপটা উপায় প্রদান করে।
