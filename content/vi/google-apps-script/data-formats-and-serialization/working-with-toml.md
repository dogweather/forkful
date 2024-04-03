---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:46.704426-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: V\xEC Google Apps Script c\u01A1 b\u1EA3\
  n l\xE0 JavaScript v\u1EDBi quy\u1EC1n truy c\u1EADp v\xE0o b\u1ED9 \u1EE9ng d\u1EE5\
  ng c\u1EE7a Google, vi\u1EC7c l\xE0m vi\u1EC7c tr\u1EF1c ti\u1EBFp v\u1EDBi TOML\
  \ trong Google\u2026"
lastmod: '2024-03-13T22:44:36.071960-06:00'
model: gpt-4-0125-preview
summary: "V\xEC Google Apps Script c\u01A1 b\u1EA3n l\xE0 JavaScript v\u1EDBi quy\u1EC1\
  n truy c\u1EADp v\xE0o b\u1ED9 \u1EE9ng d\u1EE5ng c\u1EE7a Google, vi\u1EC7c l\xE0\
  m vi\u1EC7c tr\u1EF1c ti\u1EBFp v\u1EDBi TOML trong Google Apps Script \u0111\xF2\
  i h\u1ECFi m\u1ED9t ch\xFAt s\xE1ng t\u1EA1o."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cách thực hiện:
Vì Google Apps Script cơ bản là JavaScript với quyền truy cập vào bộ ứng dụng của Google, việc làm việc trực tiếp với TOML trong Google Apps Script đòi hỏi một chút sáng tạo. Google Apps Script không hỗ trợ phân tích cú pháp TOML một cách tự nhiên, nhưng bạn có thể tận dụng các thư viện JavaScript hoặc viết một bộ phân tích cú pháp đơn giản cho các nhu cầu cơ bản.

Hãy phân tích một chuỗi cấu hình TOML đơn giản như một ví dụ:

```javascript
// Chuỗi TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Một hàm parser TOML sang JSON đơn giản
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Mục mới
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Sử dụng eval cho sự đơn giản; cảnh giác trong code sản phẩm
      currentSection[key] = value;
    }
  });
  return result;
}

// Thử nghiệm parser
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Kết quả mẫu từ `console.log` sẽ giống như một đối tượng JSON, làm cho việc truy cập các thuộc tính cấu hình trong Google Apps Script trở nên dễ dàng hơn:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Sâu hơn
TOML được tạo ra bởi Tom Preston-Werner, một trong những người sáng lập GitHub, với mục tiêu trở nên thân thiện với con người hơn JSON cho các tệp cấu hình trong khi vẫn giữ được khả năng phân tích cú pháp một cách không mơ hồ. Nó hướng tới mục tiêu càng đơn giản càng tốt, một mục tiêu phù hợp tốt với tinh thần của nhiều dự án phát triển luôn hướng tới sự đơn giản và dễ đọc trong cơ sở mã của họ.

Trong bối cảnh của Google Apps Script, việc sử dụng TOML có thể tạo ra một số công việc phức tạp, khi xem xét đến việc thiếu hỗ trợ trực tiếp và cần phải phân tích cú pháp thủ công hoặc thông qua các thư viện bên thứ ba. Đối với các dự án nhỏ hoặc những dự án không sâu rộng tích hợp vào hệ sinh thái của Google, các phương án thay thế như JSON hoặc thậm chí là cấu trúc key-value đơn giản trong cài đặt script có thể đủ và dễ thực hiện hơn. Tuy nhiên, đối với các ứng dụng ưu tiên các tệp cấu hình thân thiện với con người và đã cam kết với TOML, việc tích hợp phân tích cú pháp TOML thông qua các script tùy chỉnh thêm một lớp linh hoạt và bảo dưỡng hữu ích mà không rời bỏ các mô hình cấu hình ưa thích.
