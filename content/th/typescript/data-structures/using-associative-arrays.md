---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:32.711857-06:00
description: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D objects \u0E43\u0E19 TypeScript\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49 strings (\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E35\
  \u0E22\u0E4C) \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\
  \u0E07\u0E04\u0E39\u0E48\u0E04\u0E48\u0E32\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.936706-06:00'
model: gpt-4-0125-preview
summary: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D objects \u0E43\u0E19 TypeScript\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49 strings (\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E35\
  \u0E22\u0E4C) \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\
  \u0E07\u0E04\u0E39\u0E48\u0E04\u0E48\u0E32\u0E44\u0E14\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
---

{{< edit_this_page >}}

## อะไรและทำไม?

Associative arrays หรือ objects ใน TypeScript ช่วยให้คุณสามารถใช้ strings (หรือคีย์) เพื่อเข้าถึงคู่ค่าได้ โปรแกรมเมอร์ใช้พวกมันสำหรับรูปแบบการเข้าถึงข้อมูลที่มีความยืดหยุ่นมากขึ้นเมื่อเทียบกับอาร์เรย์แบบดั้งเดิม โดยให้วิธีการที่ยืดหยุ่นในการโครงสร้างและเข้าถึงข้อมูลโดยไม่ต้องถูกผูกมัดกับดัชนีตัวเลข

## วิธีการ:

การสร้างและใช้ associative arrays ใน TypeScript เป็นเรื่องง่าย นี่คือส่วนที่พื้นฐาน:

```TypeScript
// การประกาศ associative array
let user: { [key: string]: string } = {};

// การเพิ่มข้อมูล
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

ผลลัพธ์:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

การวนรอบผ่านคู่คีย์-ค่าก็ทำได้ง่ายเช่นกัน:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

ผลลัพธ์:

```TypeScript
name: Jane Doe
email: jane@example.com
```

และถ้าคุณกำลังจัดการกับการผสมผสานของประเภทข้อมูล TypeScript's type system มีประโยชน์:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

ผลลัพธ์:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## ลงลึก

ใน TypeScript สิ่งที่เราเรียกว่า associative arrays จริงๆ แล้วเป็น objects โดยประวัติศาสตร์ ในภาษาเช่น PHP, associative arrays เป็นประเภทพื้นฐาน แต่ JavaScript (และโดยขยาย TypeScript) ใช้ objects สำหรับวัตถุประสงค์นี้ แนวทางนี้เป็นทั้งจุดแข็งและจำกัด  Objects ให้โครงสร้างที่มีความยืดหยุ่นสูงสำหรับการเชื่อมโยงสตริงกับค่า แต่พวกมันไม่ได้ตั้งใจให้ใช้เป็น 'อาร์เรย์' ในความหมายแบบดั้งเดิม เช่น คุณไม่สามารถใช้เมทอดอาร์เรย์เช่น `push` หรือ `pop` โดยตรงกับ objects นี้

สำหรับกรณีที่คุณต้องการคอลเลกชันที่มีลำดับของคู่คีย์-ค่าพร้อมการดำเนินการแบบอาร์เรย์ TypeScript (และ JavaScript สมัยใหม่) มี `Map` object ให้:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

ในขณะที่ระบบประเภทของ TypeScript และคุณสมบัติ ES6 เช่น `Map` ให้ทางเลือกที่มีพลัง การทำความเข้าใจวิธีใช้ objects เป็น associative arrays มีประโยชน์สำหรับสถานการณ์ที่ตัวแปร object มีประสิทธิภาพสูงขึ้นหรือเมื่อทำงานกับโครงสร้างข้อมูล JSON มันเกี่ยวกับการเลือกเครื่องมือที่เหมาะสมสำหรับงาน
