---
title:                "Sinh số ngẫu nhiên"
aliases:
- /vi/cpp/generating-random-numbers.md
date:                  2024-01-28T22:01:07.368728-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc sinh số ngẫu nhiên trong lập trình liên quan đến việc tạo ra các chuỗi số không có thứ tự hoặc mẫu đoán trước được. Các lập trình viên thường sử dụng những số này cho nhiều mục đích như mô phỏng các sự kiện không lường trước được, trong kiểm tra và gỡ lỗi, và trong thuật toán trò chơi để đảm bảo công bằng hoặc tính không dự đoán.

## Cách thực hiện:

Để sinh số ngẫu nhiên trong C++, bạn thường sẽ sử dụng đến tiêu đề `<random>`, được giới thiệu trong C++11, cung cấp một loạt các tiện ích để sinh số ngẫu nhiên từ nhiều phân phối khác nhau.

```C++
#include <iostream>
#include <random>

int main() {
    // Khởi tạo một động cơ ngẫu nhiên
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Định nghĩa phạm vi [0, 99] bao gồm cả hai đầu
    std::uniform_int_distribution<> distrib(0, 99); 

    // Sinh và in ra 5 số ngẫu nhiên trong phạm vi đã định
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Mẫu mã này khởi tạo một bộ sinh số ngẫu nhiên Mersenne Twister với một hạt giống từ `std::random_device`. Sau đó, nó định nghĩa một phân phối số nguyên đồng nhất trong khoảng [0, 99] và cuối cùng in ra 5 số ngẫu nhiên từ phân phối này.

Kết quả mẫu có thể trông như thế này, nhưng hãy nhớ rằng mỗi lần thực hiện sẽ có khả năng sinh ra kết quả khác nhau:

```
45 67 32 23 88
```

## Sâu hơn:

Trong lịch sử, việc sinh số ngẫu nhiên trong C++ phụ thuộc nhiều vào hàm `rand()` và hàm `srand()` để seeding, được tìm thấy trong tiêu đề `<cstdlib>`. Tuy nhiên, phương pháp này thường xuyên bị chỉ trích vì thiếu đồng nhất và dự đoán trong phân phối số được sinh ra.

Sự giới thiệu của tiêu đề `<random>` trong C++11 đánh dấu một sự cải thiện đáng kể, cung cấp một hệ thống tinh vi để sản xuất số ngẫu nhiên. Các tiện ích được cung cấp bao gồm nhiều loại động cơ (như `std::mt19937` cho Mersenne Twister) và phân phối (như `std::uniform_int_distribution` cho phân phối đồng nhất của số nguyên) có thể được kết hợp để đáp ứng nhu cầu cụ thể của lập trình viên, dẫn đến hành vi dự đoán được, hiệu suất tốt hơn và linh hoạt hơn.

Mặc dù thư viện `<random>` tốt hơn nhiều so với phương pháp `rand()` cũ, cần lưu ý rằng việc sinh ra số thực sự ngẫu nhiên - đặc biệt cho mục đích mật mã học - vẫn phụ thuộc vào các khía cạnh khác. Đối với các ứng dụng mật mã học, nên sử dụng các thư viện được thiết kế riêng cho bảo mật, thường sử dụng các nguồn entropy phần cứng.
