---
title:                "Phân tích ngày từ chuỗi ký tự"
aliases:
- /vi/google-apps-script/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:58:36.577649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi ký tự"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do

Việc phân tích một ngày từ một chuỗi bao gồm việc chuyển đổi văn bản thể hiện ngày thành một đối tượng ngày, giúp lập trình viên thực hiện các thao tác liên quan đến ngày như so sánh, tính toán và định dạng. Điều này rất quan trọng để xử lý đầu vào của người dùng, xử lý dữ liệu từ nguồn bên ngoài và quản lý các ngày ở các định dạng khác nhau, đặc biệt là trong các ứng dụng liên quan đến lịch trình, phân tích dữ liệu hoặc bất kỳ loại hồ sơ dựa trên thời gian nào.

## Cách thực hiện:

Trong Google Apps Script, dựa trên JavaScript, bạn có nhiều cách tiếp cận để phân tích một ngày từ một chuỗi. Dưới đây là các ví dụ sử dụng cả phương thức của JavaScript bản địa và tiện ích của Google Apps Script.

**Sử dụng hàm tạo `new Date()`:**

Cách đơn giản nhất để phân tích một chuỗi thành ngày trong Google Apps Script là sử dụng hàm tạo của đối tượng `Date`. Tuy nhiên, nó yêu cầu chuỗi ngày phải ở định dạng được phương pháp Date.parse() nhận biết (ví dụ: YYYY-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Ghi nhật ký Thứ Bảy, 01 Tháng 4 2023 00:00:00 GMT+0000 (UTC)
```

**Sử dụng `Utilities.parseDate()`:**

Để linh hoạt hơn, đặc biệt là với các định dạng ngày tùy chỉnh, Google Apps Script cung cấp `Utilities.parseDate()`. Phương thức này cho phép bạn chỉ định định dạng ngày, múi giờ và ngôn ngữ.

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Ghi nhật ký Thứ Bảy, 01 Tháng 4 2023 00:00:00 GMT+0000 (UTC) tùy thuộc vào múi giờ của script
```

Lưu ý: Mặc dù `Utilities.parseDate()` cung cấp nhiều kiểm soát hơn, hành vi của nó có thể thay đổi dựa trên múi giờ của script, vì vậy điều quan trọng là phải chỉ định rõ ràng múi giờ nếu ứng dụng của bạn xử lý các ngày ở nhiều khu vực khác nhau.

## Sâu hơn

Việc phân tích ngày trong các ngôn ngữ lập trình đã lịch sử đối mặt với những thách thức, chủ yếu do sự đa dạng của các định dạng ngày và phức tạp của các múi giờ. Cách tiếp cận của Google Apps Script, chủ yếu dựa trên JavaScript, nhằm đơn giản hóa điều này bằng cách cung cấp cả đối tượng `Date` đơn giản và hàm `Utilities.parseDate()` đa năng hơn. Tuy nhiên, mỗi phương pháp đều có hạn chế của riêng nó; chẳng hạn, việc dựa vào hàm tạo `Date` với chuỗi dẫn đến không nhất quán qua các môi trường khác nhau do sự khác biệt trong cách giải thích các định dạng ngày. Mặt khác, `Utilities.parseDate()` yêu cầu hiểu biết rõ ràng về định dạng, múi giờ và ngôn ngữ, làm cho nó hơi phức tạp hơn nhưng đáng tin cậy hơn cho các nhu cầu cụ thể.

Các thư viện hoặc dịch vụ thay thế, như Moment.js (giờ đây khuyến nghị Luxon cho các dự án mới), cung cấp các chức năng phong phú hơn và xử lý múi giờ tốt hơn, giải quyết nhiều thách thức này. Tuy nhiên, trong bối cảnh của Google Apps Script, nơi các thư viện bên ngoài có hạn chế, việc hiểu và tận dụng hiệu quả các phương thức có sẵn trở nên rất quan trọng. Lập trình viên đến từ các ngôn ngữ khác có thể thấy các sắc thái trong việc xử lý ngày trong Google Apps Script là một thách thức độc đáo nhưng có thể đạt được việc phân tích ngày mạnh mẽ với sự hiểu biết sâu sắc về các công cụ có sẵn và sự cân nhắc cẩn thận về bản chất toàn cầu của ứng dụng của họ.
