---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:01:40.337399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo số ngẫu nhiên trong Ruby bao gồm việc tạo ra những số không thể dự đoán một cách logic, điều này rất quan trọng cho các kịch bản như mô phỏng, mã hóa, và trò chơi. Lập trình viên sử dụng sự ngẫu nhiên để thêm vào sự không chắc chắn hoặc mô phỏng sự biến đổi của thực tế trong ứng dụng của họ.

## Làm thế nào:

Ruby cung cấp một số phương pháp để tạo số ngẫu nhiên, chủ yếu thông qua lớp `Random`.

### Số Ngẫu Nhiên Cơ Bản

Để tạo một số ngẫu nhiên cơ bản:

```Ruby
puts rand(10) # Tạo một số ngẫu nhiên từ 0 đến 9
```

### Số Ngẫu Nhiên Trong Phạm Vi

Để có một số ngẫu nhiên trong phạm vi cụ thể:

```Ruby
puts rand(1..10) # Tạo một số ngẫu nhiên từ 1 đến 10
```

### Sử Dụng Lớp Random

Để tạo một chuỗi số ngẫu nhiên có thể lặp lại, bạn có thể sử dụng lớp `Random` với một hạt giống.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Tạo một số "ngẫu nhiên" có thể dự đoán
```

### Tạo Một Phần Tử Mảng Ngẫu Nhiên

Chọn một phần tử ngẫu nhiên từ một mảng:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Chọn ngẫu nhiên một phần tử từ mảng
```

### Kết Quả Mẫu:

Mỗi đoạn mã ở trên, khi được chạy, sẽ tạo ra các kết quả khác nhau do bản chất ngẫu nhiên của chúng. Ví dụ, `rand(10)` có thể xuất ra `7`, trong khi `colors.sample` có thể xuất ra `"green"`.

## Sâu Hơn

Khái niệm về việc tạo số ngẫu nhiên trong khoa học máy tính là một nghịch lý bởi vì máy tính tuân theo các chỉ dẫn xác định. Các phương pháp sớm phụ thuộc nhiều vào đầu vào bên ngoài để đạt được sự không chắc chắn. Sự ngẫu nhiên của Ruby được xây dựng dựa trên thuật toán Mersenne Twister, một bộ tạo số pseudo-ngẫu nhiên được biết đến với chu kỳ dài và phân phối đều, làm cho nó rất phù hợp cho các ứng dụng yêu cầu sự ngẫu nhiên chất lượng cao.

Mặc dù các phương pháp được tích hợp sẵn trong Ruby phục vụ tốt cho hầu hết nhu cầu, chúng có thể không đủ cho tất cả mục đích mã hóa, bởi vì tính dự đoán của các số pseudo-ngẫu nhiên có thể là một điểm yếu. Đối với an toàn mã hóa, các nhà phát triển Ruby có thể khám phá các thư viện như `OpenSSL::Random`, được thiết kế để tạo ra các số ngẫu nhiên an toàn về mặt mã hóa, đảm bảo sự không chắc chắn cao hơn cho các ứng dụng nhạy cảm.
