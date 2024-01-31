---
title:                "Sử dụng mảng liên kết"
date:                  2024-01-30T19:12:23.491554-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

category:             "Javascript"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, hoặc như chúng được biết đến chính xác hơn trong JavaScript, là đối tượng (objects), cho phép bạn ánh xạ các khóa với giá trị. Điều này rất tiện lợi khi bạn cần một tập hợp các phần tử mà bạn muốn truy cập thông qua các tên cụ thể (các khóa) thay vì chỉ mục số, làm cho mã của bạn trở nên dễ đọc và linh hoạt hơn.

## Cách thực hiện:

Việc tạo và sử dụng mảng kết hợp (objects) trong JavaScript rất đơn giản. Bạn định nghĩa một đối tượng với các dấu ngoặc nhọn `{}`, và bên trong đó, bạn có thể định nghĩa một tập hợp các cặp khóa-giá trị. Các khóa luôn là chuỗi, và giá trị có thể là bất kỳ thứ gì: chuỗi, số, mảng, thậm chí là các đối tượng khác.

```javascript
// Tạo một mảng kết hợp
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Truy cập các phần tử
console.log(userInfo.name); // Đầu ra: Alex
console.log(userInfo["email"]); // Đầu ra: alex@example.com

// Thêm các phần tử mới
userInfo.job = "Developer";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Đầu ra:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/

// Xóa một phần tử
delete userInfo.age;
console.log(userInfo);
/* Đầu ra:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/
```

Như bạn thấy, việc truy cập, thêm, hoặc xóa các phần tử trong một mảng kết hợp khá trực tiếp và trực quan.

## Sâu hơn nữa

Trong thế giới JavaScript, mặc dù chúng ta thường nghe thuật ngữ "mảng kết hợp," đó kỹ thuật là một tên gọi không chính xác bởi vì JavaScript không có mảng kết hợp thực sự như các ngôn ngữ khác (ví dụ: PHP). Những gì JavaScript có là các đối tượng phục vụ một mục đích tương tự nhưng là một cấu trúc mạnh mẽ và linh hoạt hơn.

Truyền thống, các mảng trong ngôn ngữ lập trình được thiết kế để chứa một tập hợp các mục, được truy cập thông qua chỉ mục số của chúng. Tuy nhiên, khi phát triển phần mềm tiến triển, nhu cầu về các cấu trúc dữ liệu linh hoạt hơn xuất hiện. Mảng kết hợp, hoặc từ điển trong các ngôn ngữ khác, là một phản ứng, cho phép truy cập vào các phần tử thông qua các khóa tùy ý.

Tiếp cận của JavaScript với đối tượng như các kho lưu trữ khóa-giá trị cung cấp một sự pha trộn chức năng. Nó cho phép các thuộc tính (các khóa) được thêm vào, loại bỏ, và tra cứu theo tên. JSON (JavaScript Object Notation) là lời chứng cho tính hữu ích của cấu trúc này, trở thành tiêu chuẩn de facto cho trao đổi dữ liệu trên web.

Mặc dù đối tượng đáp ứng hầu hết nhu cầu về mảng kết hợp, trong các trường hợp mà thứ tự khóa hoặc lặp là quan trọng, đối tượng `Map` được giới thiệu trong ES6 cung cấp một lựa chọn tốt hơn. Một `Map` giữ thứ tự khóa, chấp nhận nhiều loại dữ liệu hơn làm khóa, và bao gồm các phương thức hữu ích cho lặp và truy xuất kích thước. Mặc dù có những lợi ích này, cú pháp đối tượng truyền thống vẫn phổ biến vì sự đơn giản và dễ sử dụng của nó trong nhiều tình huống thông thường.
