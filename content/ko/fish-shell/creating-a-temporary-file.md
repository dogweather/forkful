---
title:                "임시 파일 만들기"
html_title:           "Fish Shell: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

임시 파일 생성은 프로그래머들이 프로그램을 실행하는 동안 필요한 임시 데이터를 저장하는 한 가지 방법입니다. 이를테면, 작업 중에 프로그램에서 생성한 데이터를 임시 파일에 저장하고, 작업이 끝나면 해당 파일을 삭제하는 것입니다.

## 방법:

Fish Shell에서 임시 파일을 생성하는 방법은 간단합니다. 아래의 예제 코드를 따라해 보세요.

```fish
# 임시 파일 생성
set temp (mktemp)

# 생성한 임시 파일에 데이터를 쓰기
echo "Hello, world" > $temp

# 임시 파일 내용 출력
cat $temp

# 임시 파일 삭제
rm $temp
```

위 코드를 실행하면, 임시 파일이 생성되고 해당 파일에 "Hello, world"라는 내용이 쓰여지며, 임시 파일을 삭제하면 해당 내용이 사라집니다.

## 깊이 들어가보기:

임시 파일 생성은 프로그래밍에서 자주 사용되는 기법입니다. 옛날에는 프로그램이 실행 중에 메모리가 부족하다면 임시 파일을 사용해서 데이터를 저장했었기 때문입니다. 하지만 최근에는 프로그램이 메모리를 더욱 효율적으로 사용할 수 있게 되면서 임시 파일을 사용하는 경우는 드물어졌습니다. 그래도 여전히 메모리 부족 문제를 해결하기 위해 가끔 사용되는 기법이기도 합니다. Fish Shell에서는 다른 쉘과 비교했을 때 더 간편하게 임시 파일을 생성하고 관리할 수 있습니다. 다른 대안으로는 메모리 내 테이블을 사용하는 방법이 있지만, 이는 임시 파일 생성보다 복잡하고 느릴 수 있습니다.

## 참고:

- [Fish Shell Documentation on Temporary Files](https://fishshell.com/docs/current/index.html#tempfiles)
- [Wikipedia Article on Temporary Files](https://ko.wikipedia.org/wiki/%EC%9E%84%EC%8B%9C_%ED%8C%8C%EC%9D%BC)