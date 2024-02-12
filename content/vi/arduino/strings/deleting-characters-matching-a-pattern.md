---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:58:44.769792-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Xóa ký tự khớp với một mẫu nghĩa là loại bỏ các chuỗi ký tự cụ thể khỏi chuỗi - hãy nghĩ về việc làm sạch dữ liệu hoặc đầu vào. Các lập trình viên làm điều này để chuẩn hóa, đơn giản hóa, hoặc xác nhận thông tin trước khi xử lý.

## Cách thực hiện:

Giả sử chúng ta muốn loại bỏ tất cả các chữ số số học khỏi chuỗi của chúng ta. Chúng ta có một chuỗi với một số chữ số ngẫu nhiên, và chúng ta đang hướng tới kết quả chỉ gồm chữ cái.

```Arduino
void setup() {
  Serial.begin(9600);

  // Chuỗi ban đầu của chúng ta có chứa số
  String stringWithNumbers = "Ar3du1n0 1s aw3som3!";
  String cleanedString = deletePattern(stringWithNumbers, "0123456789");

  // In chuỗi đã được làm sạch
  Serial.println(cleanedString);
}

void loop() {
  // Không có gì để thực hiện ở đây
}

String deletePattern(String str, String pattern) {
  for (unsigned int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```

Nếu bạn tải và chạy điều này trên Arduino của mình, bạn sẽ thấy chuỗi không có số trong màn hình serial:

```
Arduino is awesome!
```

## Sâu hơn nữa

Việc loại bỏ ký tự khớp với một mẫu cụ thể không phải là một khái niệm mới. Các ngôn ngữ lập trình sớm đã có các hàm để xử lý và thao tác chuỗi. Trong Arduino, mặc dù không tồn tại một hàm cấp cao cho việc xóa mẫu một cách tự nhiên, chúng ta có thể tạo ra lô-gic tùy chỉnh của mình, như trong hàm `deletePattern` ở trên.

Có những lựa chọn thay thế trong các ngôn ngữ khác, như regex (biểu thức chính quy) trong Python hoặc JavaScript, nhưng môi trường lập trình của Arduino cơ bản hơn. Nó không bao gồm các hàm regex ngay từ đầu, chủ yếu do sức mạnh xử lý và bộ nhớ giới hạn của nó.

Bên dưới lớp vỏ, hàm `deletePattern` của chúng ta lặp qua chuỗi mẫu của chúng ta, sử dụng phương thức `String.replace()` để tìm kiếm ký tự hiện tại, và thay thế nó bằng một chuỗi trống, do đó "xóa" nó khỏi chuỗi gốc của chúng ta.

## Xem thêm

- Thao tác chuỗi với Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Tham khảo String Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Thêm về thay thế chuỗi: http://www.cplusplus.com/reference/string/string/replace/