---
title:                "Làm việc với TOML"
aliases:
- /vi/google-apps-script/working-with-toml.md
date:                  2024-02-01T22:06:46.704426-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm thế nào & Tại sao?

TOML, viết tắt của Tom's Obvious, Minimal Language, là một định dạng tệp cấu hình dễ đọc do ngữ nghĩa rõ ràng của nó. Các lập trình viên thường sử dụng nó cho các tệp cấu hình trong ứng dụng bởi vì nó đơn giản và dễ đọc, giúp quản lý các cài đặt và cấu hình ứng dụng một cách liền mạch qua các môi trường khác nhau.

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
