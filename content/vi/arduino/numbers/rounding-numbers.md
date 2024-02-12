---
title:                "Làm tròn số"
aliases:
- /vi/arduino/rounding-numbers.md
date:                  2024-01-28T22:06:41.002363-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Làm tròn số là việc cắt giảm một số thập phân về giá trị nguyên gần nhất hoặc về một số lượng chữ số thập phân cố định. Các lập trình viên làm tròn số để chúng dễ đọc và xử lý hơn, đặc biệt khi độ chính xác vượt qua một mức độ nhất định không cần thiết hoặc có thể dẫn đến lỗi.

## Làm thế nào:
Trong Arduino, bạn có thể làm tròn số sử dụng các hàm có sẵn. Các hàm quan trọng là `round`, `ceil`, và `floor`. Dưới đây là một ví dụ nhanh:

```arduino
void setup() {
  Serial.begin(9600);
  
  float soCuaToi = 123.4567;

  // Làm tròn đến số nguyên gần nhất
  Serial.println(round(soCuaToi)); // Xuất ra: 123

  // Luôn làm tròn lên
  Serial.println(ceil(soCuaToi));  // Xuất ra: 124

  // Luôn làm tròn xuống
  Serial.println(floor(soCuaToi)); // Xuất ra: 123
}

void loop() {
  // Không có gì để lặp qua.
}
```

## Sâu Hơn:
Các thuật toán làm tròn có một lịch sử dài; chúng đã tồn tại trước cả khi có máy tính số. Trong máy tính tương tự, làm tròn là một quá trình vật lý. Trong máy tính số, làm tròn là một quá trình toán học.

Việc làm tròn được cần đến khi chúng ta chuyển từ một kiểu có độ chính xác cao (như `float` hay `double`) sang một kiểu có độ chính xác thấp hơn (như `int`). Nhưng cách chúng ta làm tròn có thể thay đổi:

1. `round()`: Làm tròn tiêu chuẩn. Nếu phần thập phân từ 0.5 trở lên, nó sẽ tăng lên; nếu không, nó sẽ giảm xuống.
2. `ceil()`: Viết tắt của "ceiling", luôn làm tròn lên đến số nguyên gần nhất, ngay cả khi nó gần với số thấp hơn.
3. `floor()`: Ngược lại với ceiling; luôn làm tròn xuống.

Việc chọn lựa giữa các hàm này phụ thuộc vào mục đích của giá trị làm tròn. Các phép đo có thể cần làm tròn tiêu chuẩn, tiền tệ thường sử dụng `floor`, trong khi hệ thống tồn kho có thể sử dụng `ceil` để đảm bảo mọi thứ đều được tính đến.

Việc thực thi các hàm này trong Arduino khá đơn giản; chúng không xử lý các trường hợp bổ sung như làm tròn đến số chữ số thập phân cụ thể. Đối với điều đó, một hàm tự tạo hoặc toán học sâu hơn được đưa vào cuộc chơi - nghĩ về việc nhân số lên để dịch chuyển phần thập phân, làm tròn, sau đó chia lại.

Lỗi làm tròn có thể tích tụ, ảnh hưởng đáng kể đến các tính toán dài hạn hoặc các quy trình lặp đi lặp lại. Các lập trình viên cần phải cẩn thận khi thực hiện nhiều hoạt động trên các giá trị làm tròn.

## Xem Thêm:
2. Cái nhìn sâu sắc hơn về những cạm bẫy và chiến lược cho làm tròn: [Hướng dẫn Số Chấm Động](https://floating-point-gui.de/)
3. Đối với các kỹ thuật tiên tiến, bao gồm hàm làm tròn tùy chỉnh và xử lý lỗi làm tròn, bạn có thể tham khảo các nguồn tài liệu học thuật hoặc hướng dẫn lập trình chi tiết.
