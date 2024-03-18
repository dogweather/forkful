---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:48.560213-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে আপনার Java অ্যাপ্লিকেশনের ভিতরে এই হালকা ডাটা-ইন্টারচেঞ্জ ফরম্যাটের সাথে মোকাবিলা করা। প্রোগ্রামাররা নেটওয়ার্কের উপর স্ট্রাকচার করা ডাটা সিরিয়ালাইজ এবং ট্রান্সমিট করার জন্য এবং ডাটা সহজেই কনফিগার এবং স্টোর করার জন্য জেসনের দিকে ঝুঁকে থাকে, কারণ এটি মানব-পঠনযোগ্য এবং ভাষা-স্বতন্ত্র।

## কিভাবে:
চলুন আমরা আমাদের হাতাঘাতি উঠাই এবং Java-তে JSON এর সাথে কোডিং শুরু করি।

প্রথমত, আপনার একটি JSON প্রক্রিয়াকরণ লাইব্রেরির প্রয়োজন হবে যেমন `Jackson` অথবা `Google Gson`। এখানে আমরা `Jackson` ব্যবহার করব, তাই আপনার `pom.xml`-এ এই ডিপেনডেন্সি যোগ করুন:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

এখন, চলুন আমরা একটি সাধারণ Java অবজেক্টকে JSON-এ সিরিয়ালাইজ (লিখি):

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

আউটপুট হওয়া উচিত:

```json
{"name":"Alex","age":30}
```

এখন, JSON-কে আবার Java অবজেক্টে ডিসিরিয়ালাইজ (পড়া) এর জন্য:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " এর বয়স " + person.age + " বছর।");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

আউটপুট হবে:

```
Alex এর বয়স 30 বছর।
```

## গভীরে ডুব:
JSON-এর সাধারণতা এবং কার্যকারিতা ওয়েবে ডাটা এক্সচেঞ্জের জন্য ডি ফ্যাক্টো মানদণ্ডে পরিণত করেছে, XML-কে তার সিংহাসন থেকে উল্টে দিয়েছে। ২০০০-এর দশকের প্রারম্ভে প্রবর্তিত, JSON JavaScript থেকে উদ্ভূত হলেও এখন তা বেশিরভাগ ভাষাতেই সমর্থিত।

JSON-এর বিকল্প হিসাবে XML আছে, যা বিস্তারিত ভাষা, এবং Protocol Buffers অথবা MessagePack মতো বাইনারি ফর্ম্যাট আছে, যা মানব-পঠনযোগ্যতায় কম কিন্তু আকার এবং গতিতে বেশি দক্ষ। প্রতি ফরম্যাটের ব্যবহারিক ক্ষেত্র আছে; বাছাই আপনার নির্দিষ্ট ডাটা চাহিদা এবং প্রেক্ষাপটের উপর নির্ভর করে।

Java-তে, `Jackson` এবং `Gson` ছাড়াও, আমাদের কাছে `JsonB` এবং `org.json` হিসাবে আরো লাইব্রেরি আছে যা JSON সম্পর্কিত কাজে লাগে। Jackson স্ট্রিম-ভিত্তিক প্রক্রিয়াকরণ প্রদান করে এবং গতির জন্য পরিচিত, যেখানে Gson এর ব্যবহারের সহজতার জন্য প্রশংসিত হয়। JsonB হলো Jakarta EE-র অংশ, যা আরও মানকীকৃত পদ্ধতি প্রদান করে।

JSON বাস্তবায়ন করার সময়, খারাপ ইনপুটের বিরুদ্ধে আপনার কোডকে মজবুত করার জন্য সঠিকভাবে আপনার ব্যতিক্রমগুলি সামলান - এবং স্বয়ংক্রিয় ডেটা বাইন্ডিংয়ের নিরাপত্তা প্রভাব বিবেচনা করে নিন - সবসময় আপনার ইনপুটগুলি যাচাই করুন!

## আরও দেখুন
- [Jackson প্রকল্প](https://github.com/FasterXML/jackson)
- [Gson প্রকল্প](https://github.com/google/gson)
- [JSON স্পেসিফিকেশন](https://www.json.org/json-en.html)
- [JsonB স্পেসিফিকেশন](https://jakarta.ee/specifications/jsonb/)
