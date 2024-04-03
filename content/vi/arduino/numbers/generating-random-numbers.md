---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:12.597832-07:00
description: "Vi\u1EC7c t\u1EA1o ra s\u1ED1 ng\u1EABu nhi\xEAn trong c\xE1c d\u1EF1\
  \ \xE1n Arduino li\xEAn quan \u0111\u1EBFn vi\u1EC7c s\u1EA3n xu\u1EA5t c\xE1c gi\xE1\
  \ tr\u1ECB kh\xF4ng th\u1EC3 d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc \u0111\u01B0\u1EE3\
  c theo thi\u1EBFt k\u1EBF, r\u1EA5t quan tr\u1ECDng cho\u2026"
lastmod: '2024-03-13T22:44:36.985197-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o ra s\u1ED1 ng\u1EABu nhi\xEAn trong c\xE1c d\u1EF1 \xE1\
  n Arduino li\xEAn quan \u0111\u1EBFn vi\u1EC7c s\u1EA3n xu\u1EA5t c\xE1c gi\xE1\
  \ tr\u1ECB kh\xF4ng th\u1EC3 d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc \u0111\u01B0\u1EE3\
  c theo thi\u1EBFt k\u1EBF, r\u1EA5t quan tr\u1ECDng cho c\xE1c \u1EE9ng d\u1EE5\
  ng nh\u01B0 tr\xF2 ch\u01A1i, m\xF4 ph\u1ECFng, v\xE0 h\u1EC7 th\u1ED1ng b\u1EA3\
  o m\u1EADt."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cái gì & Tại sao?
Việc tạo ra số ngẫu nhiên trong các dự án Arduino liên quan đến việc sản xuất các giá trị không thể dự đoán trước được theo thiết kế, rất quan trọng cho các ứng dụng như trò chơi, mô phỏng, và hệ thống bảo mật. Lập trình viên sử dụng kỹ thuật này để giới thiệu sự biến đổi hoặc đưa ra các quyết định không nên là quyết định xác định.

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
