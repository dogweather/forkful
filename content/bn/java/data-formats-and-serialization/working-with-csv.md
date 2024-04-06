---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:31.896382-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 CSV \u09B8\u09AE\
  \u09B0\u09CD\u09A5\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\
  \u09A4 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `java.io`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\u099C\u09C7\u0987 \u098F\u0995\
  \u099F\u09BF CSV \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964."
lastmod: '2024-03-17T18:47:43.928215-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF\u09A4\u09C7 CSV \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7\
  \ \u0986\u09AA\u09A8\u09BF `java.io` \u0995\u09CD\u09B2\u09BE\u09B8\u0997\u09C1\u09B2\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\
  \u099C\u09C7\u0987 \u098F\u0995\u099F\u09BF CSV \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:


### স্ট্যান্ডার্ড জাভা লাইব্রেরি ব্যবহার করে CSV ফাইল পড়া
জাভার স্ট্যান্ডার্ড লাইব্রেরিতে CSV সমর্থন অন্তর্ভুক্ত নেই, তবে আপনি `java.io` ক্লাসগুলি ব্যবহার করে সহজেই একটি CSV ফাইল পড়তে পারেন।

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // CSV ফাইলের পথ নির্দিষ্ট করুন
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // ধরে নিন কমা হল ডেলিমিটার
                // ডেটা প্রক্রিয়া করুন
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### স্ট্যান্ডার্ড জাভা লাইব্রেরি ব্যবহার করে CSV ফাইলে লেখা
CSV ফাইলে ডেটা লেখার জন্য, আপনি `java.io` ক্লাসগুলি যেমন `FileWriter` এবং `BufferedWriter` ব্যবহার করতে পারেন।

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // আউটপুট CSV ফাইলের পথ নির্দিষ্ট করুন

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // ধরে নিন কমা হল ডেলিমিটার
            }
            sb.deleteCharAt(sb.length() - 1); // শেষ কমা সরান
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার: Apache Commons CSV
Apache Commons CSV হল জাভায় CSV ফাইল হ্যান্ডেলিংয়ের জন্য একটি জনপ্রিয় লাইব্রেরি। এটি CSV ফাইল পড়া এবং লেখা অনেক সহজ করে দেয়।

আপনার প্রজেক্টে ডিপেন্ডেন্সি যোগ করুন:

Maven এর জন্য:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- সর্বশেষ সংস্করণের জন্য চেক করুন -->
</dependency>
```

#### CSV ফাইল পড়া:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // কলামের সূচক অনুযায়ী মান প্রাপ্তি
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### CSV ফাইলে লেখা:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"First Name", "Last Name", "Age", "City"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // এখানে Object[] হিসেবে কাস্ট করা আবশ্যক
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV ক্ষেত্রের মধ্যে কমাস এবং উদ্ধৃতি সম্পর্কিত জটিলতা যেমন স্বয়ংক্রিয়ভাবে হ্যান্ডেল করে, যা জাভাতে CSV ম্যানিপুলেশনের জন্য এটিকে একটি দৃঢ় পছন্দ করে তোলে।
