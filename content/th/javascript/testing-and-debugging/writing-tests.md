---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:28.928542-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 JavaScript \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2D\
  \u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E17\u0E35\u0E48\u0E17\u0E33\u0E01\
  \u0E32\u0E23\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\
  \u0E13\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\
  \u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\
  \u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\u0E04\u0E32\u0E14\u0E2B\u0E27\u0E31\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.604937-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 JavaScript \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2D\
  \u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E17\u0E35\u0E48\u0E17\u0E33\u0E01\
  \u0E32\u0E23\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\
  \u0E13\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\
  \u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\
  \u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\u0E04\u0E32\u0E14\u0E2B\u0E27\u0E31\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเขียนทดสอบใน JavaScript หมายถึงการสร้างสคริปต์อัตโนมัติที่ทำการรันโค้ดของคุณเพื่อให้แน่ใจว่ามันทำงานได้ตามที่คาดหวัง ซึ่งสามารถปรับปรุงความน่าเชื่อถือและความสามารถในการบำรุงรักษาของแอปพลิเคชันของคุณได้อย่างมาก โปรแกรมเมอร์ทำสิ่งนี้เพื่อจับบั๊กในช่วงต้น, ช่วยในการรีแฟกเตอร์โค้ด และให้แน่ใจว่าฟีเจอร์ใหม่ๆ ไม่ทำให้ฟังก์ชันการทำงานที่มีอยู่เดิมเสียหาย

## วิธีการ:

### วิธีพื้นฐาน (การใช้ Jest)

Jest เป็นเฟรมเวิร์คการทดสอบที่ได้รับความนิยม ซึ่งมี API ที่เป็นมิตรสำหรับการเขียน unit tests ใน JavaScript ต้องการการตั้งค่าเริ่มต้นน้อยและมาพร้อมกับฟีเจอร์เช่นฟังก์ชันทดลอง (mock functions), ตัวจับเวลา และการทดสอบสแนปช็อต

1. **การติดตั้ง**:

```bash
npm install --save-dev jest
```

2. **การเขียนทดสอบง่ายๆ**:

สร้างไฟล์ชื่อ `sum.test.js`:

```javascript
const sum = require('./sum'); // สมมุติว่าฟังก์ชันนี้เป็นการบวกเลขสองตัว

test('บวก 1 + 2 เท่ากับ 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **การรันทดสอบของคุณ**:

```bash
npx jest
```

**ผลลัพธ์ตัวอย่าง**:

```plaintext
PASS  ./sum.test.js
✓ บวก 1 + 2 เท่ากับ 3 (5ms)
```

### การทดสอบโค้ดแบบอะซิงโครนัส

Jest ทำให้การทดสอบ promises และ syntax async/await เป็นเรื่องง่าย:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('การบวกแบบอะซิงโครนัสทำงาน', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### การใช้ไลบรารีของบุคคลที่สาม (Mocha & Chai)

Mocha เป็นเฟรมเวิร์คการทดสอบอีกตัวที่ได้รับความนิยม บ่อยครั้งที่ใช้ร่วมกับไลบรารีการยืนยัน Chai เพื่อการทดสอบที่มีการแสดงออกมากขึ้น

1. **การติดตั้ง**:

```bash
npm install --save-dev mocha chai
```

2. **การเขียนทดสอบด้วย Mocha และ Chai**:

สร้าง `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // โมดูลการคำนวณง่ายๆ

describe('Calculate', function() {
  it('ควรบวกค่าสองตัว', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **การรันทดสอบของคุณด้วย Mocha**:

เพิ่มสคริปต์ใน `package.json` ของคุณ:

```json
"scripts": {
  "test": "mocha"
}
```

จากนั้นทำการรัน:

```bash
npm test
```

**ผลลัพธ์ตัวอย่าง**:

```plaintext
  Calculate
    ✓ ควรบวกค่าสองตัว


  1 passing (8ms)
```

ตัวอย่างเหล่านี้แสดงการเขียนและการทำงานของการทดสอบพื้นฐานใน JavaScript การเลือกใช้เฟรมเวิร์คการทดสอบเช่น Jest หรือ Mocha ร่วมกับ Chai สามารถให้ฐานรากที่มั่นคงสำหรับการทดสอบแอปพลิเคชันที่แข็งแกร่ง ช่วยให้แน่ใจว่าโค้ดของคุณทำงานได้ตามที่ตั้งใจไว้ตลอดการอัปเดตและการปรับปรุงโค้ด
