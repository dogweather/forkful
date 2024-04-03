---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:06.639520-07:00
description: "YAML l\xE0 m\u1ED9t chu\u1EA9n h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc v\u1EDBi con ng\u01B0\u1EDDi. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng n\xF3 cho c\xE1c t\u1EC7p c\u1EA5u h\xECnh, trao \u0111\u1ED5i d\u1EEF li\u1EC7\
  u gi\u1EEFa c\xE1c ng\xF4n ng\u1EEF, v\xE0 h\u01A1n n\u1EEFa b\u1EDFi v\xEC\u2026"
lastmod: '2024-03-13T22:44:36.346641-06:00'
model: gpt-4-0125-preview
summary: "YAML l\xE0 m\u1ED9t chu\u1EA9n h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\u1ECD\
  c v\u1EDBi con ng\u01B0\u1EDDi."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Để làm việc với YAML trong TypeScript, bạn sẽ cần một thư viện như `js-yaml`. Trước tiên, hãy cài đặt nó:

```bash
npm install js-yaml
```

Bây giờ, chuyển một chuỗi YAML thành một đối tượng JavaScript:

```typescript
import yaml from 'js-yaml';

const yamlStr = `
name: John Doe
age: 30
`;

try {
  const doc = yaml.load(yamlStr);
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Đầu ra mẫu:

```json
{ name: 'John Doe', age: 30 }
```

Để chuyển đổi một đối tượng thành một chuỗi YAML:

```typescript
import yaml from 'js-yaml';

const obj = { name: 'Jane Doe', age: 25 };

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Đầu ra mẫu:

```yaml
name: Jane Doe
age: 25
```

## Sâu hơn nữa
YAML được bắt đầu vào năm 2001, nhằm mục tiêu đọc và trao đổi dữ liệu dễ dàng giữa các ngôn ngữ. Nó là một tập mở rộng của JSON. Các lựa chọn thay thế bao gồm JSON và XML, nhưng cú pháp tối giản của YAML thường được ưu tiên cho các tệp cấu hình. Khi bạn làm việc với YAML trong TypeScript, hãy nhớ rằng nó không có kiểu dữ liệu; hãy cẩn thận với dữ liệu nhận được, đặc biệt là từ các nguồn không đáng tin cậy, để tránh các vấn đề về bảo mật.

## Xem thêm
- Trang web chính thức của YAML: http://yaml.org
- Kho lưu trữ `js-yaml` trên GitHub: https://github.com/nodeca/js-yaml
- So sánh YAML và JSON: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
