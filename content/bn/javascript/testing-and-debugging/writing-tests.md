---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:59.178604-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:44.456823-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:


### নেটিভ পদ্ধতি (Jest ব্যবহার করে)
Jest একটি জনপ্রিয় টেস্টিং ফ্রেমওয়ার্ক যা জাভাস্ক্রিপ্টে ইউনিট টেস্ট লেখার জন্য বন্ধুত্বপূর্ণ API সরবরাহ করে। এটি সর্বনিম্ন কনফিগারেশন প্রয়োজন এবং মক ফাংশন, টাইমার এবং স্ন্যাপশট টেস্টিং এর মতো বৈশিষ্ট্যগুলি সহ আসে।

১. **ইনস্টলেশন**:

```bash
npm install --save-dev jest
```

২. **একটি সহজ টেস্ট লেখা**:

`sum.test.js` নামের একটি ফাইল তৈরি করুন:

```javascript
const sum = require('./sum'); // মনে করুন এই ফাংশন কেবল দুটি সংখ্যা যোগ করে

test('1 + 2 যোগ করলে 3 হবে', () => {
  expect(sum(1, 2)).toBe(3);
});
```

৩. **আপনার টেস্ট চালানো**:

```bash
npx jest
```

**সাম্পল আউটপুট:**

```plaintext
PASS  ./sum.test.js
✓ 1 + 2 যোগ করলে 3 হয় (5ms)
```

### অ্যাসিনক্রোনাস কোড টেস্টিং
Jest প্রমিস এবং async/await সিনট্যাক্সটি টেস্ট করা সহজ করে তোলে:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('async যোগ কাজ করে', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার (Mocha & Chai)
Mocha আরেকটি জনপ্রিয় টেস্টিং ফ্রেমওয়ার্ক, যা প্রায়ই অধিক প্রকাশ্য টেস্টগুলির জন্য দাবি লাইব্রেরি Chai এর সাথে ব্যবহার করা হয়।

১. **ইনস্টলেশন**:

```bash
npm install --save-dev mocha chai
```

২. **Mocha এবং Chai ব্যবহার করে একটি টেস্ট লেখা**:

`calculate.test.js` তৈরি করুন:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // একটি সহজ গণনা মডিউল

describe('Calculate', function() {
  it('দুটি মান যোগ করা উচিত', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

৩. **Mocha সহ আপনার টেস্ট চালানো**:

আপনার `package.json` এ একটি স্ক্রিপ্ট যোগ করুন:

```json
"scripts": {
  "test": "mocha"
}
```

তারপর চালান:

```bash
npm test
```

**সাম্পল আউটপুট:**

```plaintext
  Calculate
    ✓ দুটি মান যোগ করা উচিত


  1 passing (8ms)
```

এই উদাহরণগুলি জাভাস্ক্রিপ্টে মৌলিক টেস্ট লেখা এবং নির্বাহের কিছু নিদর্শন দেয়। Jest বা Mocha এর মতো একটি টেস্টিং ফ্রেমওয়ার্ক গ্রহণ করে Chai এর সাথে রোবাস্ট অ্যাপ্লিকেশন টেস্টিংয়ের একটি দৃঢ় ভিত্তি প্রদান করতে পারে, এটি সহায়তা করে আপডেট এবং রিফ্যাক্টরিংয়ের মাধ্যমে আপনার কোড যেমন অভিপ্রেত সেভাবে কাজ করে তা নিশ্চিত করতে।
