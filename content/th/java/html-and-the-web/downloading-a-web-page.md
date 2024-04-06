---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:46.449902-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E2D\u0E32\u0E08\
  \u0E21\u0E35\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\u0E14\u0E31\u0E07\u0E19\u0E35\u0E49\
  ."
lastmod: '2024-04-05T21:54:01.682606-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C\u0E2D\u0E32\u0E08\u0E21\u0E35\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\
  \u0E14\u0E31\u0E07\u0E19\u0E35\u0E49."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String urlStr = "http://example.com";
        try {
            URL url = new URL(urlStr);
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    System.out.println(line);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

ตัวอย่างผลลัพธ์อาจมีลักษณะดังนี้:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## ลงลึก
ในอดีต, การดาวน์โหลดเว็บเพจเป็นเรื่องง่าย—HTTP นั้นเรียบง่าย, เว็บไซต์มีเพียง HTML ส่วนใหญ่เป็นแบบสแตติก วันนี้เว็บไซต์ซับซ้อน—นึกถึง HTTPS, เนื้อหาที่ขับเคลื่อนด้วย JavaScript, และ AJAX เต็มไปหมด

สำหรับเนื้อหาแบบสแตติก, `java.net.URL` และ `java.net.HttpURLConnection` เป็นตัวเลือกที่ตรงไปตรงมา—ไม่มีเครื่องเยอะ, แค่ใช้งานได้ แต่ถ้าคุณต้องการเว็บไซต์ที่เต็มไปด้วยเนื้อหาแบบไดนามิกที่โหลดโดย JavaScript, คลาสเหล่านั้นอาจจะไม่พอ, และคุณอาจจะต้องมองหาเครื่องมืออย่าง Selenium หรือ HtmlUnit แทน

อย่าลืม, การเลือกเครื่องมือที่เหมาะสมยังขึ้นอยู่กับว่าคุณต้องการทำอะไรกับเว็บเพจหลังจากที่ดาวน์โหลดมาแล้ว การแยกคำ HTML? Jsoup เป็นเพื่อนของคุณ การดำเนินการ JavaScript? พิจารณาเบราว์เซอร์แบบไม่มีหน้าต่าง คลาส `java.net` เป็นเพียงจุดเริ่มต้นของธารน้ำแข็ง, แต่มีความเหมาะสมสำหรับงานที่รวดเร็วหรือการขูดข้อมูลจากเว็บเพจธรรมดาๆ

จำนโยบายของความสุภาพ: อย่าทำการร้องขอแบบรวดเร็วต่อเว็บไซต์, หรือคุณจะถูกแบน และตรวจสอบให้แน่ใจว่าคุณปฏิบัติตามแนวทาง `robots.txt` ของเว็บไซต์อย่างเหมาะสม

## ดูเพิ่มเติม
- ห้องสมุด [Jsoup](https://jsoup.org/) สำหรับการประมวลผลและการสกัด HTML
- [Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver/) สำหรับงานที่ซับซ้อนขึ้น รวมถึงการดำเนินการ JavaScript
- คู่มือสำหรับ [HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) สำหรับผู้ที่ต้องการรายละเอียดลึกล้ำเกี่ยวกับวิธีการจัดการ HTTP ที่ใช้ใน Java
- [HtmlUnit](http://htmlunit.sourceforge.net/), "เบราว์เซอร์ที่ไม่มี GUI สำหรับโปรแกรม Java", เหมาะสำหรับหน้าเว็บที่มี JavaScript มาก
