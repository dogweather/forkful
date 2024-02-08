---
title:                "Trích xuất chuỗi con"
date:                  2024-02-01T21:53:42.165591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/extracting-substrings.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc trích xuất các chuỗi con bao gồm việc lấy một phần của chuỗi - cơ bản là tạo một chuỗi mới từ một phần của chuỗi hiện có. Lập trình viên thực hiện điều này vì nhiều lý do, bao gồm việc phân tích dữ liệu, thao tác văn bản cho giao diện người dùng, hoặc xử lý nhập liệu cho các ứng dụng khác nhau, khiến việc trích xuất chuỗi con trở thành một công cụ linh hoạt trong bất kỳ bộ công cụ lập trình nào.

## Làm sao:

Trong Google Apps Script, dựa trên JavaScript hiện đại, việc trích xuất chuỗi con có thể đạt được thông qua vài phương thức, bao gồm `substring()`, `substr()`, và `slice()`. Mỗi phương thức đều có những nét đặc thù, nhưng tất cả đều phục vụ mục đích lấy các ký tự đã chỉ định từ một chuỗi.

```javascript
// Ví dụ sử dụng substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Đầu ra: Hello

// Ví dụ sử dụng substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Đầu ra: world

// Ví dụ sử dụng slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Đầu ra: world!
```

Mỗi phương thức đều nhận hai đối số: vị trí bắt đầu và, trừ `slice()` có thể chấp nhận chỉ số âm để bắt đầu từ phía sau, vị trí kết thúc hoặc số ký tự cần trích xuất. Đáng chú ý là chuỗi gốc vẫn không thay đổi sau những thao tác này, vì chúng trả về các giá trị chuỗi mới.

## Đi sâu vào vấn đề

Trong lịch sử, các phương thức JavaScript để trích xuất các chuỗi con đã là nguồn cơn nhầm lẫn do tên gọi và chức năng tương tự nhau. Tuy nhiên, trong Google Apps Script và JavaScript hiện đại, `substring()` và `slice()` thường được sử dụng nhiều nhất, với `substr()` được coi là lỗi thời. Điều này quan trọng cần lưu ý cho những ai viết mã lập trình với tư duy hướng tới tương lai.

Sự khác biệt chính giữa `substring()` và `slice()` là cách chúng xử lý chỉ số âm; `substring()` coi chỉ số âm là 0, trong khi `slice()` có thể chấp nhận chỉ số âm để bắt đầu việc trích xuất từ cuối chuỗi. Điều này làm cho `slice()` đặc biệt hữu ích trong các trường hợp có thể không biết chính xác độ dài của chuỗi hoặc khi cần trích xuất từ cuối.

Khi quyết định phương thức nào để sử dụng cho việc trích xuất chuỗi con, sự lựa chọn thường phụ thuộc vào yêu cầu cụ thể của thao tác (ví dụ, liệu việc xử lý chỉ số âm có lợi không) và tiêu chuẩn mã hóa cá nhân hoặc nhóm. Mặc dù không có quy tắc nào phù hợp với mọi trường hợp, việc hiểu được những khác biệt tinh tế và hậu quả về hiệu năng có thể giúp đưa ra quyết định thông báo.
