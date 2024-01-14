---
title:    "Python: 임시 파일 만들기"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 왜
임시 파일을 만드는 일에 참여하는 이유는 우리 모두가 가끔은 프로그램을 실행해서 임시적인 데이터를 저장하고 싶거나 다른 프로그램에서 사용하고 싶을 때가 있기 때문입니다.

## 어떻게
임시 파일을 생성하는 방법은 아주 간단합니다. 아래의 코드 블록을 참고해주세요.

```python
import tempfile

# 임시 파일 생성
temp_file = tempfile.TemporaryFile()
print(temp_file.name)

# 임시 파일에 데이터 쓰기
temp_file.write(b"This is a temporary file. It will be deleted automatically.")

# 임시 파일에서 데이터 읽기
temp_file.seek(0)
data = temp_file.read()
print(data)

# 임시 파일 자동 삭제 확인
print("Is the temporary file still available? " + str(not temp_file.closed))
```

위의 코드를 실행하면 아래와 같은 결과가 출력됩니다.

```
/var/folders/5r/7cns346j18753b34h8t4yw8h0000gn/T/tmpctwy3khc
b'This is a temporary file. It will be deleted automatically.'
Is the temporary file still available? False
```

## 깊이 파헤치기
임시 파일 생성에 대해 더욱 깊이 알아보겠습니다. 파이썬의 `tempfile` 모듈은 운영 체제에 따라 다른 임시 디렉토리를 사용하는 등 다양한 설정을 할 수 있습니다. 또한, 사용이 끝나면 자동으로 삭제되므로 파일 관리에 대한 부담을 덜어줍니다. 이 모듈은 매우 유용하고 효율적이므로 임시 파일 생성에 주로 사용됩니다.

## 참고
- [Python Docs - tempfile](https://docs.python.org/3.9/library/tempfile.html)
- [GeeksforGeeks - Python | tempfile](https://www.geeksforgeeks.org/python-tempfile-module/)