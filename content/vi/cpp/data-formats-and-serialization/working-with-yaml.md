---
title:                "Làm việc với YAML"
aliases:
- vi/cpp/working-with-yaml.md
date:                  2024-01-28T22:11:34.296956-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Làm việc với YAML đề cập đến việc phân tích và tạo dữ liệu trong ngôn ngữ YAML Ain't Markup Language thân thiện với con người. Lập trình viên sử dụng nó cho các tệp cấu hình, chuỗi hóa dữ liệu và cài đặt ứng dụng do độ dễ đọc và đơn giản của nó.

## Làm thế nào:

YAML không được tích hợp sẵn trong C++. Bạn sẽ cần một thư viện như `yaml-cpp`. Dưới đây là cách phân tích một tệp YAML đơn giản:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("config.yaml");
    YAML::Node config = YAML::Load(file);
    
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();
    
    std::cout << "Tên: " << username << ", Tuổi: " << age << std::endl;
    return 0;
}
```

Giả sử `config.yaml` là:
```
user:
  name: John Doe
  age: 30
```

Kết quả:
```
Tên: John Doe, Tuổi: 30
```

## Tìm hiểu sâu hơn

YAML được giới thiệu lần đầu tiên vào năm 2001 như một tiêu chuẩn chuỗi hóa dữ liệu dễ đọc cho con người. Trong khi JSON và XML là các lựa chọn phổ biến khác, cú pháp tối thiểu của YAML đã làm cho nó trở nên phổ biến cho các tệp cấu hình. Các thư viện như `yaml-cpp` xử lý việc phân tích và phát sinh dữ liệu YAML, biểu diễn nó trong các cấu trúc như bản đồ và chuỗi, tương tự như đối tượng và mảng JSON.

## Xem thêm

- Đặc tả YAML 1.2: https://yaml.org/spec/1.2/spec.html
- Kho GitHub yaml-cpp: https://github.com/jbeder/yaml-cpp
- Giới thiệu về YAML: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
