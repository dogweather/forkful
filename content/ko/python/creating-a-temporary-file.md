---
title:                "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
임시 파일을 만드는 것에 참여하는 이유는 다양합니다. 가장 일반적인 이유는 임시적으로 데이터를 저장하거나 처리할 때 필요합니다. 또 다른 이유는 시스템 자원을 효율적으로 관리하기 위해 사용될 수 있습니다.

## 어떻게
파이썬에서 임시 파일을 만드는 것은 간단합니다. 아래의 예제 코드를 참고하세요.

```Python
import tempfile

# 임시 파일 생성
tmp_file = tempfile.NamedTemporaryFile()

# 파일 경로 확인
print(tmp_file.name)

# 임시 파일에 데이터 쓰기
tmp_file.write(bytes("안녕하세요!", encoding="utf-8"))

# 임시 파일에서 데이터 읽기
tmp_file.seek(0)
print(tmp_file.read())

# 임시 파일 닫기
tmp_file.close()
```

위의 예제 코드를 실행하면 임시 파일이 생성되고 파일 경로가 출력됩니다. 임시 파일에 데이터를 쓰고 읽은 후, 파일을 닫아줍니다. 이제 필요 없는 임시 파일은 자동으로 삭제됩니다.

## 깊게 파헤치기
Python에서 임시 파일을 만드는 방법에는 `tempfile` 모듈의 `NamedTemporaryFile()` 함수가 있습니다. 이 함수는 `TemporaryFile()` 클래스의 인스턴스를 반환합니다. `TemporaryFile()` 클래스는 `NamedTemporaryFile()` 함수와 동일한 기능을 제공하지만, 이름을 제공하지 않고 시스템에서 임의의 이름으로 파일이 생성됩니다.

임시 파일 생성 후, 해당 파일에 데이터를 쓰고 읽는 것은 일반적인 파일 작업과 동일합니다. 다만 파일을 닫을 때에는 `close()` 메서드 대신 `__exit__()` 메서드를 호출해야 합니다. 이렇게 하면 임시 파일이 자동으로 삭제됩니다.

여러 파일을 생성할 경우, `tempfile` 모듈을 사용하여 특정 디렉토리 내에 임시 파일을 생성할 수도 있습니다. 또한, 임시 파일 생성시 사용할 파일 이름을 직접 정할 수도 있습니다.

## 참고 자료
- [파이썬 공식 문서 - tempfile 모듈](https://docs.python.org/ko/3/library/tempfile.html)
- [Python for Beginner - 파일 입출력 - 임시 파일](https://python.flowdas.com/howto/files.html#solving-your-needs-using-temporary-files)
- [Python Weekly - How do I manipulate temporary files and directories with Python?](https://blog.pythonweekly.xyz/manipulating-temporary-files-and-directories/)