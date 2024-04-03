---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:07.368728-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEA\
  n trong C++, b\u1EA1n th\u01B0\u1EDDng s\u1EBD s\u1EED d\u1EE5ng \u0111\u1EBFn ti\xEA\
  u \u0111\u1EC1 `<random>`, \u0111\u01B0\u1EE3c gi\u1EDBi thi\u1EC7u trong C++11,\
  \ cung c\u1EA5p m\u1ED9t lo\u1EA1t c\xE1c ti\u1EC7n \xEDch\u2026"
lastmod: '2024-03-13T22:44:37.037619-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEAn trong C++, b\u1EA1n th\u01B0\
  \u1EDDng s\u1EBD s\u1EED d\u1EE5ng \u0111\u1EBFn ti\xEAu \u0111\u1EC1 `<random>`,\
  \ \u0111\u01B0\u1EE3c gi\u1EDBi thi\u1EC7u trong C++11, cung c\u1EA5p m\u1ED9t lo\u1EA1\
  t c\xE1c ti\u1EC7n \xEDch \u0111\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEAn t\u1EEB nhi\u1EC1\
  u ph\xE2n ph\u1ED1i kh\xE1c nhau."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

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
