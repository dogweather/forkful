---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:05.466835-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1\
  i m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh\
  \ vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 d\u1ECDn d\u1EB9p c\u01A1 s\u1EDF m\xE3,\u2026"
lastmod: '2024-02-25T18:49:35.454757-07:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i\
  \ m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh\
  \ vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 d\u1ECDn d\u1EB9p c\u01A1 s\u1EDF m\xE3,\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên làm điều này để dọn dẹp cơ sở mã, cải thiện khả năng đọc, bảo trì và mở đường cho các tính năng tương lai với ít nợ kỹ thuật nhất có thể.

## Làm thế nào:
Hãy bắt đầu với một ví dụ cơ bản trong Swift, nơi chúng ta có một số mã lặp lại:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Họ: \(firstName)")
    print("Tên: \(lastName)")
    print("Tuổi: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Chức danh công việc: \(title)")
    print("Công ty: \(company)")
}
```

Việc tái cấu trúc này sẽ bao gồm việc tạo một cấu trúc `User` để đóng gói các thuộc tính người dùng và thêm một phương thức để in chi tiết:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Họ: \(firstName)")
        print("Tên: \(lastName)")
        print("Tuổi: \(age)")
        print("Chức danh công việc: \(jobTitle)")
        print("Công ty: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Nhà phát triển phần mềm", company: "Giải pháp công nghệ")
user.printDetails()
```

### Đầu ra mẫu:
```
Họ: John
Tên: Doe
Tuổi: 30
Chức danh công việc: Nhà phát triển phần mềm
Công ty: Giải pháp công nghệ
```

## Sâu hơn
Tái cấu trúc có nguồn gốc từ những ngày đầu của kỹ thuật phần mềm, nhưng thuật ngữ được làm phổ biến vào cuối những năm 1990, đặc biệt qua cuốn sách quan trọng "Tái cấu trúc: Cải thiện Thiết kế Mã nguồn Hiện có" của Martin Fowler. Cuốn sách đã đề ra nguyên tắc mã nguồn nên được dọn dẹp liên tục trong các bước nhỏ thay vì chờ đợi một giai đoạn riêng biệt.

Các lựa chọn thay thế cho việc tái cấu trúc thủ công bao gồm công cụ tự động và môi trường phát triển tích hợp (IDEs) có thể giúp phát hiện mã trùng lặp, đề xuất sự đơn giản hóa và tự động tạo ra phần của mã. Xcode, cho phát triển Swift, cung cấp các công cụ tái cấu trúc khác nhau, như đổi tên và chức năng trích xuất phương thức, có thể giảm tiềm năng cho lỗi con người trong quá trình này.

Khi thực hiện tái cấu trúc, quan trọng là phải có một bộ kiểm thử vững chắc. Các kiểm thử hoạt động như một lưới an toàn, đảm bảo rằng những thay đổi mà bạn đang thực hiện không đưa ra lỗi. Điều này rất quan trọng vì mục tiêu chính của việc tái cấu trúc là thay đổi cấu trúc bên trong mà không ảnh hưởng đến hành vi bên ngoài.

## Xem thêm
- ["Refactoring: Improving the Design of Existing Code" bởi Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Tài liệu Swift bởi Apple](https://swift.org/documentation/)
- [Sử dụng công cụ tái cấu trúc Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Hướng dẫn phong cách Swift của Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
