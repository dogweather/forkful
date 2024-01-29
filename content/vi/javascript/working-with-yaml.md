---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:12:11.479201-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm thế nào & Tại sao?
YAML (YAML ain't Markup Language), hay còn gọi là YAML, là một tiêu chuẩn hóa dữ liệu thân thiện với con người dành cho tất cả các ngôn ngữ lập trình. Lập trình viên làm việc với YAML bởi vì nó dễ đọc và viết, thường được sử dụng cho các tệp cấu hình, và trao đổi dữ liệu giữa các ngôn ngữ hoặc dịch vụ.

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
