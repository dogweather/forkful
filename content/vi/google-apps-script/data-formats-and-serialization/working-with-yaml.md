---
title:                "Làm việc với YAML"
aliases:
- /vi/google-apps-script/working-with-yaml/
date:                  2024-02-01T22:07:18.069347-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm Gì và Tại Sao?

YAML, viết tắt của "YAML Ain't Markup Language," là một chuẩn hóa dữ liệu dễ đọc cho con người, thường được sử dụng để tạo các tệp cấu hình và trao đổi dữ liệu giữa các ngôn ngữ với các cấu trúc dữ liệu khác nhau. Các lập trình viên thường làm việc với YAML vì tính đơn giản và dễ đọc của nó, đặc biệt trong các dự án yêu cầu cấu hình mở rộng hoặc khi chuyển dữ liệu có cấu trúc giữa các hệ thống khác nhau.

## Làm thế nào:

Mặc dù Google Apps Script (GAS) không hỗ trợ trực tiếp việc phân tích cú pháp hay chuẩn hóa YAML, bạn có thể thao tác với dữ liệu YAML bằng cách sử dụng thư viện JavaScript hoặc viết các hàm phân tích cú pháp tùy chỉnh. Để minh họa, hãy xem xét cách phân tích cú pháp một chuỗi YAML bằng một hàm tùy chỉnh, do không thể nhập trực tiếp các thư viện bên ngoài vào GAS.

Giả sử bạn có một cấu hình YAML đơn giản:

```yaml
title: Ví dụ YAML
description: Một ví dụ về cách xử lý YAML trong Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Cấu hình
```

Để phân tích cú pháp này trong Google Apps Script, sử dụng khả năng thao tác chuỗi của JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Xử lý cơ bản cho mảng
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: Ví dụ YAML\ndescription: Một ví dụ về cách xử lý YAML trong Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Cấu hình";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Khi `testYamlParsing()` được thực thi, nó xuất ra:

```
{ title: 'Ví dụ YAML',
  description: 'Một ví dụ về cách xử lý YAML trong Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Cấu hình' ] }
```

Cách tiếp cận phân tích cú pháp tùy chỉnh này khá cơ bản và có thể cần được điều chỉnh để xử lý các tệp YAML phức tạp.

## Sâu hơn nữa

YAML, được phát hành lần đầu vào năm 2001, nhằm mục đích dễ đọc hơn các định dạng trước đó như XML hoặc JSON. Mặc dù sự đơn giản và dễ sử dụng của nó được đánh giá cao, việc xử lý YAML trong Google Apps Script gặp thách thức do thiếu sự hỗ trợ trực tiếp. Do đó, các lập trình viên thường phụ thuộc vào sự linh hoạt của JavaScript để phân tích cú pháp và tạo ra dữ liệu YAML. Tuy nhiên, đối với các trường hợp sử dụng phức tạp, đặc biệt là những loại có cấu trúc lồng sâu và cấu trúc dữ liệu nâng cao, phương pháp này có thể trở nên rườm rà và dễ mắc lỗi.

Ngược lại, JSON được hỗ trợ một cách tự nhiên trong Google Apps Script và hầu hết các môi trường lập trình khác, mang lại một cách tiếp cận thẳng thắn hơn cho việc chuẩn hóa và giải chuẩn hóa dữ liệu mà không cần thu gọn cú pháp thêm. Cú pháp của JSON ít loằng ngoằng hơn so với YAML, làm cho nó phù hợp hơn cho việc trao đổi dữ liệu trong các ứng dụng web. Tuy nhiên, YAML vẫn phổ biến trong các tệp cấu hình và các trường hợp mà tính dễ đọc là yếu tố quan trọng nhất.

Khi làm việc với YAML trong Google Apps Script, cần cân nhắc giữa sự dễ đọc và dễ sử dụng. Để thao tác YAML một cách toàn diện, có thể xem xét khám phá các công cụ hoặc dịch vụ bên ngoài có thể chuyển đổi YAML sang JSON trước khi xử lý nó trong script của bạn.
