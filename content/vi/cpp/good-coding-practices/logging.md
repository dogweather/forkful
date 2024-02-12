---
title:                "Ghi log"
aliases:
- vi/cpp/logging.md
date:                  2024-01-28T22:03:36.925220-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Trong bối cảnh lập trình, việc ghi log là quá trình ghi lại các sự kiện, trạng thái và thông tin vào một tập tin hoặc một phương tiện xuất khác. Lập trình viên ghi log để theo dõi những gì đang xảy ra trong ứng dụng của họ, để gỡ lỗi và để theo dõi hiệu năng cho phân tích và tối ưu hóa trong tương lai.

## Làm Thế Nào:
Giả sử bạn đang làm việc trên một hệ thống Linux và bạn muốn đưa các file log của mình vào một tập tin bằng cách sử dụng C++ cổ điển. Bạn sẽ muốn bao gồm thư viện `<iostream>` và `<fstream>` để thực hiện các thao tác với tập tin. Dưới đây là một ví dụ nhanh:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Mở ở chế độ ghi thêm

    if (!logFile.is_open()) {
        std::cerr << "Có vấn đề khi mở tập tin log!" << std::endl;
        return 1;
    }

    logFile << "Ứng dụng đã được khởi động" << std::endl;
  
    // ... ở đâu đó trong logic ứng dụng của bạn
    logFile << "Một sự kiện quan trọng đã xảy ra" << std::endl;

    // Đừng quên đóng luồng tập tin của bạn
    logFile.close();

    return 0;
}
```

Nếu bạn theo dõi tập tin log của mình với `tail -f appLog.txt`, bạn sẽ thấy:

```
Ứng dụng đã được khởi động
Một sự kiện quan trọng đã xảy ra
```

Tuyệt vời, bạn đã có một bản ghi có dấu thời gian của các sự kiện!

## Sâu Hơn
Việc ghi log cũ kỹ như chính việc tính toán, với nguồn gốc từ những dấu vết cụ thể trên giấy để theo dõi những gì những máy tính cổ đại đang làm. Trong kỷ nguyên hiện đại, mọi thứ đều về các giải pháp phần mềm tinh vi. Bạn có ghi log trực tiếp vào tập tin, như ví dụ nhanh và dễ dàng phía trên, hoặc bạn có thể tham gia vào một framework ghi log tinh vi hơn, như Log4cpp hoặc Boost.Log trong lĩnh vực C++; những công cụ mạnh mẽ này cung cấp cấp độ log, kiểm soát định dạng và nhiều hơn nữa.

Nói về cấp độ, các phương pháp hay nhất của việc ghi log bao gồm sử dụng các cấp độ nghiêm trọng khác nhau—thông tin, gỡ lỗi, cảnh báo, lỗi, nghiêm trọng—để bạn có thể lọc bỏ tiếng ồn khi bạn đang cố gắng sửa lỗi hoặc tìm hiểu tại sao ứng dụng của mình lại có hành vi giống như một thiếu niên tâm trạng.

Về mặt hiệu năng, đừng làm lơ với log của bạn. Việc ghi log quá mức có thể chuyển ứng dụng nhanh như chớp của bạn thành một cuộc marathon ốc sên, làm nặng hệ thống tập tin, hoặc thậm chí có thể tốn kém chi phí lưu trữ nếu bạn dùng dịch vụ đám mây. Việc tìm được sự cân bằng phù hợp là chìa khóa: ghi log những gì bạn cần, và không gì hơn.

## Xem Thêm
Cho những ai muốn đi xa hơn với thực hành ghi log của mình, hãy xem:

- Thư viện [Boost.Log](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) cho một số tính năng ghi log chất lượng cao.
- [Thư viện glog của Google](https://github.com/google/glog) nếu bạn quan tâm đến việc sử dụng ứng dụng log như thế nào của gã khổng lồ công nghệ này.
- [Thư viện Log4cpp](http://log4cpp.sourceforge.net/) cho một cơ chế ghi log có thể cấu hình.

Và cho một chút đọc thêm về lý do và cách thức của việc ghi log, tham khảo:

- Chủ đề trên Stack Overflow về [các phương pháp ghi log tốt nhất](https://stackoverflow.com/questions/783956/logging-best-practices) sẽ cho bạn cái nhìn sâu rộng và được đánh giá bởi cộng đồng về chủ đề này.
