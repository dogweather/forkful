---
title:                "Ruby: 디렉토리의 존재 여부 확인하기"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

'디렉토리가 존재하는지 확인하려면'이라는 주제에 대해 이야기하기 전에, 우리는 왜 이를 알아야 하는지에 대해 생각해야 합니다. 디렉토리의 존재 여부를 확인하는 것은 나중에 사용할 파일 또는 다른 디렉토리의 위치를 알 수 있기 때문에 중요합니다. 이러한 정보를 알아내는 것은 프로그래밍에서 매우 유용한 기능입니다.

## 어떻게

Ruby에서 디렉토리가 존재하는지 확인하는 방법은 매우 간단합니다. 'File' 클래스의 'exist?' 메소드를 사용하면 됩니다. 아래 코드 블록에서는 'my_directory'라는 디렉토리가 있는지 확인하는 예제를 보여줍니다.

```Ruby
if File.exist?("my_directory")
  puts "my_directory exists"
else
  puts "my_directory does not exist"
end
```

위 코드를 실행하면 "my_directory exists"라는 결과가 출력됩니다. 만약 디렉토리가 존재하지 않는다면 "my_directory does not exist"가 출력됩니다. 이것만으로도 디렉토리의 존재 여부를 쉽게 확인할 수 있습니다.

## 심화

하지만 디렉토리가 존재하는지 확인하는 것은 그것만으로는 충분하지 않을 수 있습니다. 때로는 디렉토리가 실제로 파일이나 다른 디렉토리를 포함하고 있는지 확인해야 할 때도 있습니다. 이 경우에는 'File' 클래스의 'directory?' 메소드를 사용하면 됩니다. 아래 코드는 'my_directory' 디렉토리가 파일과 디렉토리를 포함하고 있는지 확인하는 예제입니다.

```Ruby
if File.exist?("my_directory") && File.directory?("my_directory")
  puts "my_directory is a valid directory"
else
  puts "my_directory is not a valid directory"
end
```

위 코드의 결과는 "my_directory is a valid directory"가 출력됩니다. 이렇게 함으로써 디렉토리의 존재만 아니라 디렉토리 내부에 무엇이 있는지도 확인할 수 있습니다.

## 관련 링크

- [Ruby의 'File' 클래스](https://ruby-doc.org/core-3.0.0/File.html)
- [다른 디렉토리와 파일 관련 메소드](https://ruby-doc.org/core-3.0.0/File.html#method-c-direntry)
- [디렉토리 관리에 대한 공식 Ruby 문서](https://ruby-doc.org/stdlib-3.0.0/libdoc/fileutils/rdoc/index.html)