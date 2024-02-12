---
title:                "Bắt đầu một dự án mới"
aliases: - /vi/ruby/starting-a-new-project.md
date:                  2024-01-28T22:09:29.157717-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Khởi đầu một dự án mới giống như việc bạn đang gieo hạt vào vườn kỹ thuật số của mình—bạn đang bắt đầu với một bó ý tưởng mới, và biến chúng thành code thực hiện một điều gì đó hữu ích. Các lập trình viên bắt đầu dự án mới để giải quyết vấn đề, khám phá các khái niệm, hoặc chỉ đơn giản vì niềm vui sáng tạo điều gì đó mới mẻ.

## Làm thế nào:

Vậy, bạn đã sẵn sàng biến những chớp nhoáng trong đầu thành dự án Ruby? Hãy bắt đầu. Bắt đầu với những điều cơ bản.

```Ruby
# Cài đặt Ruby, nếu bạn chưa cài.
# Kiểm tra phiên bản Ruby để đảm bảo bạn đang cập nhật:
$ ruby -v

# Đầu ra phải là phiên bản hiện tại của Ruby:
# ruby 3.x.x

# Tiếp theo, hãy tạo một thư mục cho dự án của bạn:
$ mkdir my_new_project
$ cd my_new_project

# Bây giờ, khởi tạo một kho chứa Git mới nếu bạn muốn quản lý phiên bản (rất khuyến khích):
$ git init

# Sau đó tạo một tệp nhập, hãy gọi là 'app.rb':
$ touch app.rb

# Bắt đầu lập trình! Viết một đầu ra đơn giản để đảm bảo nó đang hoạt động:
puts "Hello New Project!"

# Chạy tệp của bạn:
$ ruby app.rb

# Đầu ra phải là:
# Hello New Project!
```

## Sâu hơn nữa

Ngày xưa, khởi đầu một dự án Ruby mới khá là thô sơ—chỉ có bạn, một trình chỉnh sửa văn bản, và một đống tệp `.rb`. Khi ngôn ngữ này phát triển, đã có công cụ xuất hiện để làm cho quy trình này trôi chảy hơn.

Ví dụ, Bundler quản lý các gem—thư viện Ruby—của bạn, do đó bạn có thể theo dõi và cài đặt các phụ thuộc một cách dễ dàng. Chỉ cần chạy `bundle init` sau khi bạn thiết lập thư mục dự án của mình để tạo một `Gemfile`, nơi bạn liệt kê các gem của mình.

Sau đó, chúng ta có Ruby Version Manager (RVM) và Ruby Environment (rbenv), những công cụ giúp chuyển đổi giữa các phiên bản Ruby theo dự án. Rất tiện lợi nếu bạn đang gặp rắc rối với mã cũ.

Và còn về các framework? Ruby on Rails là cái tên lớn cho ứng dụng web. Nhưng nếu bạn muốn gọn nhẹ (như cho dịch vụ hay API), hãy thử kiểm tra Sinatra hoặc Roda.

## Xem thêm

- Trang chính thức của Ruby để cập nhật và tài liệu: [https://www.ruby-lang.org](https://www.ruby-lang.org)
- Bundler, để quản lý các gem Ruby của bạn: [https://bundler.io](https://bundler.io)
- RVM, một Trình quản lý Phiên bản Ruby: [https://rvm.io](https://rvm.io)
- rbenv, để chọn một phiên bản Ruby cho dự án của bạn: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- Sinatra, một framework web nhẹ: [http://sinatrarb.com](http://sinatrarb.com)
- Đối với việc chia sẻ và cộng tác code, GitHub là nơi bạn nên đến: [https://github.com](https://github.com)
