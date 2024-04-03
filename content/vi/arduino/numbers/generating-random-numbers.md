---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:12.597832-07:00
description: "L\xE0m th\u1EBF n\xE0o: Arduino cung c\u1EA5p c\xE1c h\xE0m \u0111\u01A1\
  n gi\u1EA3n \u0111\u1EC3 t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn: `randomSeed()` v\xE0\
  \ `random()`. \u0110\u1EC3 b\u1EAFt \u0111\u1EA7u, h\xE3y kh\u1EDFi t\u1EA1o b\u1ED9\
  \ sinh s\u1ED1 ng\u1EABu nhi\xEAn \u0111\u1EC3 \u0111\u1EA3m\u2026"
lastmod: '2024-03-13T22:44:36.985197-06:00'
model: gpt-4-0125-preview
summary: "Arduino cung c\u1EA5p c\xE1c h\xE0m \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm thế nào:
Arduino cung cấp các hàm đơn giản để tạo số ngẫu nhiên: `randomSeed()` và `random()`. Để bắt đầu, hãy khởi tạo bộ sinh số ngẫu nhiên để đảm bảo các dãy số khác nhau mỗi lần chương trình của bạn thực thi. Một phương pháp thường được sử dụng là khởi tạo với một đọc analog từ một chân không kết nối.

```Arduino
void setup() {
  Serial.begin(9600);
  // Khởi tạo giá trị hạt giống ngẫu nhiên
  randomSeed(analogRead(0));
}

void loop() {
  // Tạo một số ngẫu nhiên từ 0 đến 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Đợi một giây để dễ đọc kết quả
}
```

Chương trình trên khởi tạo máy phát số ngẫu nhiên trong hàm `setup()` và tạo ra một số mới từ 0 đến 99 trong mỗi lần lặp, xuất số đó ra Màn hình Nối tiếp.

Kết quả mẫu:
```
42
17
93
...
```

## Tìm hiểu sâu
Hàm `random()` của Arduino bên dưới sử dụng máy phát số ngẫu nhiên giả (PRNG), theo dõi một dãy xác định nhưng trông như thống kê ngẫu nhiên. Giá trị ban đầu, hay giá trị hạt giống, của dãy ảnh hưởng đáng kể đến sự không dự đoán được của nó, do đó việc sử dụng chung `randomSeed()` với một đầu vào khá ngẫu nhiên là một điểm khởi đầu. Quan trọng là lưu ý rằng tính ngẫu nhiên được tạo ra bởi Arduino là đủ cho hầu hết các dự án của người hâm mộ nhưng có thể không đáp ứng được tiêu chí cho các ứng dụng bảo mật cao do tính dự đoán được của nó theo thời gian. Đối với các mục đích mật mã học, nên xem xét các thuật toán phức tạp hơn và máy phát số ngẫu nhiên phần cứng (HRNGs), có thể cung cấp tính ngẫu nhiên thực sự bằng cách sử dụng các quá trình vật lý.
