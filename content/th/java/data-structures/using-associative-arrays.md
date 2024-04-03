---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:16.438183-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Java \u0E44\u0E21\u0E48\u0E21\
  \u0E35 associative arrays \u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\
  \u0E21\u0E01\u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\
  \u0E35\u0E48\u0E1A\u0E32\u0E07\u0E20\u0E32\u0E29\u0E32\u0E17\u0E33, \u0E41\u0E15\
  \u0E48\u0E21\u0E31\u0E19\u0E21\u0E35 `Map` interface \u0E41\u0E25\u0E30\u0E04\u0E25\
  \u0E32\u0E2A\u0E40\u0E0A\u0E48\u0E19 `HashMap` \u0E41\u0E25\u0E30 `TreeMap` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E21\u0E32\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E41\u0E17\
  \u0E19\u2026"
lastmod: '2024-03-17T21:57:56.073574-06:00'
model: gpt-4-0125-preview
summary: "Java \u0E44\u0E21\u0E48\u0E21\u0E35 associative arrays \u0E17\u0E35\u0E48\
  \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E1A\u0E32\u0E07\u0E20\u0E32\u0E29\
  \u0E32\u0E17\u0E33, \u0E41\u0E15\u0E48\u0E21\u0E31\u0E19\u0E21\u0E35 `Map` interface\
  \ \u0E41\u0E25\u0E30\u0E04\u0E25\u0E32\u0E2A\u0E40\u0E0A\u0E48\u0E19 `HashMap` \u0E41\
  \u0E25\u0E30 `TreeMap` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E21\u0E32\u0E43\u0E0A\u0E49\
  \u0E07\u0E32\u0E19\u0E41\u0E17\u0E19 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\
  \u0E18\u0E35\u0E43\u0E0A\u0E49 `HashMap`."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## วิธีการ:
Java ไม่มี associative arrays ที่มาพร้อมกับภาษาอย่างที่บางภาษาทำ, แต่มันมี `Map` interface และคลาสเช่น `HashMap` และ `TreeMap` เพื่อมาใช้งานแทน นี่คือวิธีใช้ `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // การสร้าง HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // การเพิ่มสมาชิก
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // การเข้าถึงสมาชิก
        System.out.println("อายุของ Alice: " + ageOfFriends.get("Alice"));
        
        // การจัดการกับคีย์ที่ไม่มีอยู่
        System.out.println("อายุของคนที่ไม่อยู่ใน map: " + ageOfFriends.getOrDefault("Dan", -1));

        // การวนซ้ำผ่านสมาชิก
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " อายุ " + entry.getValue() + " ปี");
        }
    }
}
```

ผลลัพธ์ตัวอย่าง:

```
อายุของ Alice: 24
อายุของคนที่ไม่อยู่ใน map: -1
Alice อายุ 24 ปี
Bob อายุ 30 ปี
Charlie อายุ 28 ปี
```

`HashMap` เป็นเพียงหนึ่งในการใช้งาน ถ้าคีย์ของคุณเป็นเอกลักษณ์และคุณต้องการให้มีการเรียงลำดับ, ลองพิจารณา `TreeMap` สำหรับ map ที่รักษาลำดับของการใส่, `LinkedHashMap` เป็นตัวเลือกที่ดีสำหรับคุณ

## การศึกษาลึก
Maps ใน Java เป็นส่วนหนึ่งของ Collections Framework ที่ได้รับการแนะนำใน JDK 1.2, แต่ได้รับการปรับปรุงอย่างมากเมื่อเวลาผ่านไป รวมถึงการนำเสนอวิธีการ `forEach` ใน Java 8 เพื่อการวนซ้ำผ่านรายการได้ง่ายขึ้น การเลือกการใช้งาน map (เช่น `HashMap`, `LinkedHashMap`, `TreeMap`) ควรถูกกำหนดโดยความต้องการที่เฉพาะเจาะจงของคุณในแง่ของการเรียงลำดับและประสิทธิภาพ ตัวอย่างเช่น, `HashMap` เสนอการทำงานในเวลา O(1) สำหรับการดำเนินการพื้นฐาน (get และ put), โดยสมมติว่าฟังก์ชันแฮชกระจายสมาชิกได้อย่างเหมาะสมในถัง อย่างไรก็ตาม, ถ้าคุณต้องการการเรียงลำดับตามลำดับธรรมชาติหรือตัวเปรียบเทียบที่กำหนดเอง, `TreeMap` คือตัวเลือกที่ไป โดยเสนอเวลาสำหรับการแทรกและค้นหาใน O(log n)

ก่อนที่ `Map` จะถูกนำเสนอ, associative arrays มักจะถูกระบุด้วยอาร์เรย์ขนานสองอาร์เรย์ (หนึ่งสำหรับคีย์, อีกหนึ่งสำหรับค่า) หรือโครงสร้างข้อมูลที่กำหนดเองที่มีความมีประสิทธิภาพน้อยกว่า ตัวเลือกอื่นที่เป็นไปได้สำหรับ `Map` และการใช้งานของมันอาจรวมถึงไลบรารีของบุคคลที่สามที่เสนอ maps พิเศษ เช่น bidirectional maps (BiMap ในไลบรารี Guava ของ Google) สำหรับกรณีที่คุณต้องการค้นหาคีย์โดยค่าได้อย่างมีประสิทธิภาพ อย่างไรก็ตาม, สำหรับกรณีการใช้งานส่วนใหญ่ใน Java, maps ของไลบรารีมาตรฐานมีความแข็งแรงและยืดหยุ่นพอที่จะจัดการงานได้
