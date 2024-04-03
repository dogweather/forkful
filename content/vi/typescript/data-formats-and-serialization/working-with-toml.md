---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:29.738232-07:00
description: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng h\xF3a d\u1EEF li\u1EC7u gi\u1ED1ng nh\u01B0\
  \ JSON ho\u1EB7c YAML. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC t\xED\
  nh d\u1EC5 \u0111\u1ECDc v\xE0 \xE1nh\u2026"
lastmod: '2024-03-13T22:44:36.350536-06:00'
model: gpt-4-0125-preview
summary: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng h\xF3a d\u1EEF li\u1EC7u gi\u1ED1ng nh\u01B0\
  \ JSON ho\u1EB7c YAML."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cách thực hiện:
Đầu tiên, bạn cần một bộ phân tích cú pháp TOML. `@iarna/toml` là một lựa chọn phổ biến. Cài đặt nó với npm: `npm install @iarna/toml --save`. Dưới đây là cách bạn đọc và phân tích cú pháp một tệp TOML thành một đối tượng JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Nếu `config.toml` chứa:
```
[server]
port = 8080
```
Kết quả sẽ là:
```
{ server: { port: 8080 } }
```
Và, viết vào một tệp TOML cũng dễ dàng tương tự:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Chạy mã này sẽ ghi đối tượng vào `config.toml` theo định dạng TOML.

## Sâu hơn nữa
TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập GitHub, vào khoảng năm 2013 như một phản ứng với những hạn chế mà ông nhận thấy trong các định dạng khác như INI hoặc YAML. Nó được thiết kế để không mơ hồ và dễ dàng phân tích cú pháp thành các cấu trúc dữ liệu, do đó, là một sự lựa chọn yêu thích cho các tệp cấu hình. Các lựa chọn khác như JSON thiếu chú thích, trong khi YAML phức tạp hơn. TOML nổi bật với sự đơn giản và khả năng mô tả rõ ràng các hệ thống dữ liệu phức tạp.

Bên dưới, khi bạn phân tích cú pháp TOML trong TypeScript, bạn đang chuyển đổi dữ liệu văn bản thành một định dạng có cấu trúc mà ngôn ngữ có thể thao tác. Điều này bao gồm việc phân tích từ vựng (chuyển văn bản thô thành các token) và phân tích cú pháp (xây dựng một cấu trúc dữ liệu nội bộ); `@iarna/toml` xử lý cả hai một cách liền mạch. Sự hỗ trợ emoji là một nét thú vị, cho thấy cách tiếp cận tập trung vào người dùng của TOML.

## Xem thêm
- Thông số kỹ thuật chính thức của TOML: https://toml.io/en/
- Gói `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- So sánh giữa TOML, YAML, và JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
