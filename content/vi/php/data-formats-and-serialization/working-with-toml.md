---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:17.385278-07:00
description: "C\xE1ch th\u1EE9c: \u0110\u1EA7u ti\xEAn, h\xE3y \u0111\u1EA3m b\u1EA3\
  o b\u1EA1n \u0111\xE3 c\xE0i \u0111\u1EB7t th\u01B0 vi\u1EC7n ph\xE2n t\xEDch TOML,\
  \ nh\u01B0 `yosymfony/toml`. Ch\xFAng ta h\xE3y ph\xE2n t\xEDch m\u1ED9t t\u1EC7\
  p TOML."
lastmod: '2024-03-13T22:44:36.798222-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ti\xEAn, h\xE3y \u0111\u1EA3m b\u1EA3o b\u1EA1n \u0111\xE3\
  \ c\xE0i \u0111\u1EB7t th\u01B0 vi\u1EC7n ph\xE2n t\xEDch TOML, nh\u01B0 `yosymfony/toml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cách thức:
Đầu tiên, hãy đảm bảo bạn đã cài đặt thư viện phân tích TOML, như `yosymfony/toml`. Chúng ta hãy phân tích một tệp TOML:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

Kết quả mẫu:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## Sâu hơn nữa
TOML ra đời vào năm 2013, được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, như một lựa chọn thân thiện với người dùng hơn so với XML và JSON cho các tệp cấu hình. Trong khi JSON đơn giản cho máy móc, cấu trúc của TOML lại dễ chịu với mắt người, không có sự phức tạp của YAML.

Các lựa chọn thay thế cho TOML bao gồm JSON, YAML và XML. Mỗi định dạng có những ưu điểm và kịch bản ứng dụng riêng. JSON phổ biến và độc lập với ngôn ngữ; YAML dễ đọc hơn và hỗ trợ bình luận, trong khi XML rộng rãi và được hỗ trợ rộng rãi.

Khi triển khai TOML trong PHP, bạn đang xem xét các thư viện phân tích nội dung của nó thành các mảng hoặc đối tượng PHP. `yosymfony/toml` là một phân tích cú pháp PHP tuân thủ với phiên bản v0.4.0 của TOML spec. Để cập nhật với những cái mới nhất, luôn kiểm tra các phân tích cú pháp mới hơn hoặc các bản cập nhật hỗ trợ phiên bản TOML mới nhất (v1.0.0 tính đến lần cập nhật cuối của tôi).

## Xem thêm
- Đặc tả TOML: <https://toml.io/>
- Phân tích cú pháp TOML cho PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- So sánh các định dạng dữ liệu (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Quản lý Gói PHP (Composer): <https://getcomposer.org/>
