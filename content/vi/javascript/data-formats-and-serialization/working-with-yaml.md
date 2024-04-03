---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:11.479201-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng th\u01B0\
  \ vi\u1EC7n `js-yaml` ph\u1ED5 bi\u1EBFn \u0111\u1EC3 ph\xE2n t\xEDch YAML th\xE0\
  nh c\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng JavaScript v\xE0 chuy\u1EC3n \u0111\u1ED5\
  i c\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng JavaScript\u2026"
lastmod: '2024-03-13T22:44:37.179741-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `js-yaml` ph\u1ED5\
  \ bi\u1EBFn \u0111\u1EC3 ph\xE2n t\xEDch YAML th\xE0nh c\xE1c \u0111\u1ED1i t\u01B0\
  \u1EE3ng JavaScript v\xE0 chuy\u1EC3n \u0111\u1ED5i c\xE1c \u0111\u1ED1i t\u01B0\
  \u1EE3ng JavaScript th\xE0nh YAML."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Chúng ta sẽ sử dụng thư viện `js-yaml` phổ biến để phân tích YAML thành các đối tượng JavaScript và chuyển đổi các đối tượng JavaScript thành YAML.

1. Đầu tiên, cài đặt thư viện:

```bash
npm install js-yaml
```

2. Phân tích YAML sang JavaScript:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const doc = yaml.load(fs.readFileSync('config.yml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Đầu ra mẫu nếu `config.yml` là:

```yaml
version: 1
services:
  - webapp
  - database
```

Có thể trông như:

```javascript
{ version: 1, services: [ 'webapp', 'database' ] }
```

3. Chuyển đổi JavaScript sang YAML:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

let data = {
  title: "Ví dụ YAML",
  description: "YAML dễ dàng"
};

try {
  const ymlText = yaml.dump(data);
  fs.writeFileSync('example.yml', ymlText, 'utf8');
} catch (e) {
  console.error(e);
}
```

Điều này sẽ tạo ra một tệp `example.yml` với:

```yaml
title: Ví dụ YAML
description: 'YAML dễ dàng'
```

## Tìm hiểu sâu hơn
YAML được bắt đầu vào năm 2001, được thiết kế để dễ đọc bởi con người và dễ viết bằng tay. JSON và XML là các lựa chọn thay thế nhưng không dễ dàng cho con người như vậy. Sự đơn giản của YAML có thể dẫn đến các vấn đề về an ninh nếu không được triển khai đúng cách, như giữ `!!python/object/apply` bị vô hiệu hóa để ngăn chặn việc thực thi mã tùy ý. Các thư viện như `js-yaml` cung cấp các tùy chọn để tùy chỉnh việc phân tích và chuyển đổi của YAML để thêm bảo mật và chức năng.

## Xem thêm
- Tiêu chuẩn YAML 1.2: https://yaml.org/spec/1.2/spec.html
- Kho GitHub js-yaml: https://github.com/nodeca/js-yaml
- Bài viết về YAML trên Wikipedia để biết thêm thông tin nền: https://en.wikipedia.org/wiki/YAML
- So sánh JSON và YAML: https://phoenixnap.com/kb/yaml-vs-json
