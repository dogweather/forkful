---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:41.383444-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi\
  \ TOML trong JavaScript, b\u1EA1n s\u1EBD c\u1EA7n m\u1ED9t b\u1ED9 ph\xE2n t\xED\
  ch c\xFA ph\xE1p nh\u01B0 `@iarna/toml`. \u0110\u1EA7u ti\xEAn, c\xE0i \u0111\u1EB7\
  t n\xF3: `npm install\u2026"
lastmod: '2024-03-13T22:44:37.183368-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi TOML trong JavaScript, b\u1EA1n s\u1EBD\
  \ c\u1EA7n m\u1ED9t b\u1ED9 ph\xE2n t\xEDch c\xFA ph\xE1p nh\u01B0 `@iarna/toml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cách thực hiện:
Để làm việc với TOML trong JavaScript, bạn sẽ cần một bộ phân tích cú pháp như `@iarna/toml`. Đầu tiên, cài đặt nó: `npm install @iarna/toml`. Sau đó, phân tích một chuỗi TOML thành đối tượng JavaScript hoặc chuyển đổi một đối tượng JavaScript thành định dạng TOML.

```javascript
const toml = require('@iarna/toml');

// Phân tích chuỗi TOML thành đối tượng JS
const tomlStr = `
title = "Ví dụ TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Chuyển đổi đối tượng JS thành chuỗi TOML
const jsObject = {
  title: "Ví dụ TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Sâu hơn
TOML được phát hành lần đầu vào năm 2013 bởi Tom Preston-Werner, một người đồng sáng lập GitHub. Nó được thiết kế để thay thế các định dạng khác, như INI, bằng cách trở nên tiêu chuẩn hóa và dễ phân tích hơn. JSON và YAML là các lựa chọn thay thế nhưng có thể quá phức tạp hoặc quá linh hoạt. Ưu điểm của TOML nằm ở cấu hình tĩnh nơi một định dạng đơn giản, rõ ràng được ưa chuộng. Thiết kế của nó cho phép ánh xạ đơn giản vào một bảng băm, với các khóa và giá trị tương ứng với tên thuộc tính và giá trị của chúng. Để được chấp nhận rộng rãi hơn, bạn có thể cần tích hợp các công cụ có thể chuyển đổi giữa TOML và các định dạng khác do sự hỗ trợ khác nhau của hệ sinh thái.

## Xem thêm
- Kho lưu trữ chính thức của TOML trên GitHub: https://github.com/toml-lang/toml
- So sánh TOML với YAML và JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- gói npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
