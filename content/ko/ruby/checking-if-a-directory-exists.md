---
title:    "Ruby: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

모두가 적절한 파일과 데이터를 찾을 수 있도록 컴퓨터에서 디렉토리의 존재여부를 확인하는 것이 중요합니다.

## 어떻게

디렉토리의 존재여부를 확인하기 위해서는 Ruby의 `File` 클래스와 `File` 모듈을 사용해야 합니다. 아래 예제 코드를 참고해보세요.

```Ruby
# 디렉토리 경로 설정
dir_path = "/Users/Username/documents"

# 디렉토리 존재여부 확인
if File.directory?(dir_path)
  puts "해당 디렉토리는 존재합니다."
else
  puts "해당 디렉토리는 존재하지 않습니다."
end
```

위 코드의 출력 결과는 다음과 같을 것입니다.

```
해당 디렉토리는 존재합니다.
```

위와 같이 `File.directory?` 메소드를 사용하면 해당 경로의 디렉토리 존재여부를 쉽게 확인할 수 있습니다.

## 딥 다이브

Ruby에서 디렉토리의 존재여부를 확인하는 방법은 여러 가지가 있습니다. 예를 들어, `Dir.exist?`과 `File.exist?` 메소드를 사용하는 방법도 있습니다. 하지만 `File.directory?` 메소드를 사용하면 특정 경로가 디렉토리인지 확인할 수 있는 더 효율적인 방법입니다. 또한 `File` 모듈의 `exist?` 메소드를 사용하면 해당 경로의 파일이나 디렉토리가 존재하는지를 확인할 수 있습니다.

## 참고

- [Ruby File 클래스 문서](https://ruby-doc.org/core-2.6/File.html)
- [Ruby File 모듈 문서](https://ruby-doc.org/core-2.6/File.html)
- [Ruby 파일과 디렉토리 관리하기](https://www.rubyguides.com/2015/07/ruby-file/)