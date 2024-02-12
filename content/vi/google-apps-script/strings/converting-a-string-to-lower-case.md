---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/google-apps-script/converting-a-string-to-lower-case.md
date:                  2024-02-01T21:51:42.530748-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi một chuỗi thành chữ thường trong Google Apps Script, một ngôn ngữ lập trình dựa trên đám mây để tự động hóa các tác vụ trên các sản phẩm của Google, là một nhiệm vụ cơ bản nhằm chuẩn hóa dữ liệu văn bản. Các lập trình viên thường thực hiện hành động này để đảm bảo tính nhất quán trong nhập liệu của người dùng, xử lý dữ liệu, hoặc khi so sánh các chuỗi, vì nó loại bỏ vấn đề về phân biệt chữ hoa chữ thường.

## Làm thế nào:

Việc chuyển đổi một chuỗi thành chữ thường trong Google Apps Script khá đơn giản, nhờ vào các phương thức JavaScript tích hợp sẵn trong môi trường lập trình. Phương thức `toLowerCase()` là cái bạn sẽ sử dụng nhiều nhất. Dưới đây là cách bạn có thể triển khai nó:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // Xuất ra: hello, world!
}
```

Hàm đơn giản này minh họa việc lấy một chuỗi gốc, áp dụng phương thức `toLowerCase()`, và ghi nhật ký kết quả. Điều này đặc biệt hữu ích khi xử lý các đầu vào cần không phân biệt chữ hoa chữ thường. Ví dụ, so sánh các địa chỉ email mà người dùng có thể nhập bằng các trường hợp khác nhau.

Thêm vào đó, trong trường hợp bạn đang làm việc với dữ liệu mảng, bạn có thể ánh xạ qua từng phần tử để chuyển chúng thành chữ thường:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // Xuất ra: [alice, bob, charlie]
}
```

Ví dụ này nhấn mạnh sự linh hoạt của `toLowerCase()` khi xử lý nhiều dữ liệu chuỗi, đảm bảo sự đồng nhất trên toàn bộ tập dữ liệu của bạn.

## Sâu hơn nữa

Phương thức `toLowerCase()`, kế thừa từ JavaScript và được sử dụng trong Google Apps Script, đã là một phần không thể thiếu của việc thao tác chuỗi kể từ các phiên bản đầu của JavaScript. Mục đích chính của nó là hỗ trợ xử lý dữ liệu văn bản không phân biệt chữ hoa chữ thường, một nhu cầu phát sinh với sự xuất hiện của các ứng dụng web tương tác người dùng động. Dù đơn giản, cơ chế này đóng một vai trò quan trọng trong việc xác nhận dữ liệu, sắp xếp, và các thuật toán tìm kiếm bằng cách giảm bớt sự phức tạp do phân biệt chữ hoa chữ thường đem lại.

Về mặt hiệu suất, quá trình chuyển đổi được tối ưu hóa cao trong các trình xử lý JavaScript hiện đại; tuy nhiên, việc áp dụng của nó vẫn nên được xem xét cẩn thận trong các hoạt động dữ liệu quy mô lớn để tránh tạo ra gánh nặng xử lý không cần thiết.

Một phương án khác để cân nhắc, đặc biệt khi làm việc với các mẫu phức tạp hoặc cần chuyển đổi cụ thể theo địa phương, là phương thức `toLocaleLowerCase()`. Biến thể này xem xét các quy tắc cụ thể của địa phương để chuyển các ký tự thành chữ thường, có thể là thiết yếu cho các ứng dụng hỗ trợ nhiều ngôn ngữ:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // Xuất ra: märz
```

Mặc dù có thêm phức tạp, `toLocaleLowerCase()` là một công cụ mạnh mẽ cho các ứng dụng quốc tế, đảm bảo rằng quá trình chuyển đổi tôn trọng các quy tắc ngôn ngữ của người dùng địa phương. Bất kể phương pháp nào bạn chọn, việc chuyển đổi chuỗi thành chữ thường vẫn là một phần cốt lõi của việc xử lý văn bản trong Google Apps Script, thu hẹp khoảng cách giữa đầu vào của người dùng và xử lý dữ liệu chuẩn hóa.
