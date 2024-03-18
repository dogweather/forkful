---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:46.981919-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 TypeScript \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E04\u0E48\u0E32\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E04\u0E32\u0E14\u0E40\u0E14\u0E32\u0E44\u0E14\u0E49\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E0A\u0E48\u0E27\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u2026"
lastmod: '2024-03-17T21:57:55.940110-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 TypeScript \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E04\u0E48\u0E32\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E04\u0E32\u0E14\u0E40\u0E14\u0E32\u0E44\u0E14\u0E49\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E0A\u0E48\u0E27\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E2A\u0E38\u0E48\u0E21"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การสร้างตัวเลขสุ่มใน TypeScript หมายถึงการสร้างค่าตัวเลขที่ไม่สามารถคาดเดาได้ภายในช่วงที่กำหนด โปรแกรมเมอร์ใช้ประโยชน์จากตัวเลขสุ่มเหล่านี้เพื่อหลากหลายวัตถุประสงค์ เช่น การสร้างรหัสประจำตัวที่ไม่ซ้ำกัน, การจำลองข้อมูลเพื่อการทดสอบ, หรือเพิ่มความไม่คาดเดาให้กับเกมและการจำลอง

## วิธีการ:

ใน TypeScript คุณสามารถสร้างตัวเลขสุ่มได้โดยใช้วัตถุ `Math` ทั่วโลก ด้านล่างเป็นตัวอย่างที่ใช้งานได้จริงในการสร้างตัวเลขสุ่มสำหรับความต้องการต่างๆ

### การสร้างตัวเลขสุ่มพื้นฐาน

เพื่อการสร้างตัวเลขทศนิยมสุ่มระหว่าง 0 (รวม) และ 1 (ไม่รวม) คุณใช้ `Math.random()` การดำเนินการนี้ไม่ต้องการการจัดการเพิ่มเติม:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

อาจให้ผลลัพธ์เช่น `0.8995452185604771`.

### การสร้างตัวเลขเต็มสุ่มระหว่างสองค่า

เมื่อคุณต้องการตัวเลขเต็มระหว่างสองค่าที่ระบุ คุณต้องใช้ `Math.random()` ร่วมกับเลขคณิตบางอย่าง:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

อาจให้ผลลัพธ์เป็นค่าเต็มระหว่าง 1 และ 10 เช่น `7`.

### การสร้างรหัสประจำตัวที่ไม่ซ้ำ

ตัวเลขสุ่มสามารถรวมกับวิธีอื่นๆ เพื่อสร้างรหัสประจำตัวที่ไม่ซ้ำ ตัวอย่างเช่น ส่วนของโค้ดผู้สร้าง UUID ง่ายๆ:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

สิ่งนี้สร้างสตริงที่คล้ายกับ UUID เช่น `110e8400-e29b-41d4-a716-446655440000`.

## การศึกษาอย่างลึกซึ้ง

วิธีการหลักในการสร้างตัวเลขสุ่มใน JavaScript และใน TypeScript, `Math.random()`, อาศัยตัวสร้างตัวเลขสุ่มเทียม (PRNG) สำคัญที่จะทราบว่า แม้ว่าผลลัพธ์อาจดูเป็นสุ่ม แต่พวกเขาถูกสร้างขึ้นโดยอัลกอริธึมประจำตัวที่พึ่งพาค่าเมล็ดเริ่มต้น ดังนั้น ตัวเลขที่สร้างโดย `Math.random()` จึงไม่ใช่ตัวเลขสุ่มที่แท้จริงและไม่ควรใช้สำหรับวัตถุประสงค์ทางการเข้ารหัส

สำหรับตัวเลขสุ่มที่ปลอดภัยตามการเข้ารหัส, Web Crypto API นำเสนอ `crypto.getRandomValues()`, ซึ่งสามารถเข้าถึงได้ในสภาพแวดล้อมที่รองรับมาตรฐาน Web Crypto รวมถึงเบราว์เซอร์สมัยใหม่และ Node.js (ผ่านโมดูล `crypto`) นี่คือตัวอย่างที่ชัดเจนแสดงการใช้งานใน TypeScript เพื่อการสร้างตัวเลขสุ่มที่ปลอดภัยในช่วง:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

วิธีนี้ให้ระดับของความสุ่มที่แข็งแกร่งกว่าและมีความเหมาะสมมากขึ้นสำหรับแอปพลิเคชันที่มีความไวต่อความปลอดภัย อย่างไรก็ตาม, วิธีนี้ยังใช้ทรัพยากรมากขึ้นและอาจไม่จำเป็นสำหรับงานที่ง่ายขึ้นเช่น การจำลองง่ายๆ หรือการสร้างค่าสุ่มที่ไม่สำคัญ
