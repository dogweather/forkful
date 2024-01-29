---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:32.551639-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quá trình chỉnh sửa lại mã của bạn để làm cho nó sạch sẽ hơn, dễ bảo trì hơn, mà không thay đổi hành vi bên ngoài của nó. Các lập trình viên tái cấu trúc để cải thiện khả năng đọc, giảm độ phức tạp, và làm cho cơ sở mã dễ dàng hơn để cập nhật hoặc thêm tính năng trong tương lai.

## Làm thế nào:
Giả sử bạn có một đoạn mã mà bạn đang thực hiện một số tính toán hoặc thao tác chuỗi lặp đi lặp lại trên nhiều hàm. Đó là mục tiêu lớn cho việc tái cấu trúc. Dưới đây là trước và sau khi sử dụng Gleam, có một trọng tâm mạnh mẽ về tính an toàn kiểu và tính bất biến:

```gleam
// Trước khi tái cấu trúc
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("Diện tích là \(area)")
}

// Sau khi tái cấu trúc
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("Diện tích là \(area)")
}

// Trong một phần khác của mã của bạn, bạn sẽ gọi print_area như sau:
print_area(calculate_area(10, 20))
```

Đầu ra mẫu:
```
Diện tích là 200
```

Bằng cách tái cấu trúc, chúng tôi đã làm cho `print_area` tập trung hơn vào việc in, trong khi việc tính toán được xử lý ở nơi khác, làm cho mã dễ sử dụng và kiểm tra hơn.

## Đi sâu hơn
Tái cấu trúc, như một khái niệm, đã tồn tại từ khi lập trình ra đời—xem xét và dọn dẹp mã là một phần của việc giữ gìn nhà cửa tốt. Cái hiện đại hóa của tái cấu trúc, cùng với nhiều kỹ thuật và mô hình được sử dụng ngày nay, có thể quay về cuốn sách quan trọng của Martin Fowler "Refactoring: Improving the Design of Existing Code" xuất bản năm 1999.

Trong hệ sinh thái Gleam, tái cấu trúc có các xem xét cụ thể. Một trong những điều quan trọng nhất là kiểm tra kiểu mạnh mẽ tại thời gian biên dịch, có thể giúp phát hiện lỗi sớm khi bạn di chuyển các thành phần. Các tính năng khớp mẫu và tính bất biến của Gleam cũng có thể hướng dẫn bạn viết mã rõ ràng hơn, ngắn gọn hơn—một trong những mục tiêu chính của việc tái cấu trúc.

Các phương án thay thế cho việc tái cấu trúc có thể bao gồm viết lại mã từ đầu hoặc vá mã với các sửa đổi nhanh chóng. Tuy nhiên, tái cấu trúc thường là cách tiếp cận an toàn và hiệu quả nhất để cải thiện mã hiện có mà không giới thiệu lỗi mới, vì nó liên quan đến những thay đổi tăng tiến, được đánh dấu rõ ràng, bảo toàn hành vi.

## Xem thêm
- Cuốn sách "Refactoring" của Martin Fowler: https://martinfowler.com/books/refactoring.html
- Trang web ngôn ngữ Gleam, với thêm tài liệu và ví dụ: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" của Martin Fowler (cho các nguyên tắc cơ bản áp dụng cho các ngôn ngữ khác): https://martinfowler.com/books/refactoring.html
