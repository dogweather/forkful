---
title:    "Ruby: Sprawdzanie czy istnieje katalog"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego?

Często w trakcie pisania programów w języku Ruby możemy potrzebować sprawdzenia, czy dany katalog istnieje. Może to wynikać z różnych powodów, na przykład chcemy upewnić się, że katalog, do którego chcemy zapisać pliki, istnieje lub szukamy plików w konkretnym katalogu. W takich sytuacjach sprawdzenie istnienia katalogu może być bardzo pomocne, dlatego warto poznać kilka sposobów na to, jak to zrobić.

## Jak to zrobić?

Aby sprawdzić, czy dany katalog istnieje, możemy skorzystać z metody `Dir.exist?` lub `File.directory?`. Pierwsza z nich zwraca wartość logiczną `true` lub `false`, w zależności od tego, czy katalog istnieje, a druga zaś zwraca obiekt typu `File` lub `nil`. Poniżej znajdują się przykładowe kody wykorzystujące te metody:

```Ruby
Dir.exist?("folder") 
# => true if "folder" exists, false if it doesn't

File.directory?("folder") 
# => returns "File" object if "folder" exists, nil if it doesn't
```

Możemy również wykorzystać operator logiczny `&&`, aby połączyć obie metody i uzyskać dokładniejszy wynik:

```Ruby
File.directory?("folder") && Dir.exist?("folder")
# => will only return true if "folder" exists and it is a directory
```

## Głębszy zanurzenie

Warto również wiedzieć, że poprzednie metody korzystają z systemowego wywołania `stat`, co oznacza, że jeśli mamy dostęp do połączenia SSH, możemy użyć jej do sprawdzenia istnienia katalogu na zdalnym serwerze. Możemy to zrobić, wykorzystując gem `net-ssh`:

```Ruby
require 'net/ssh'

Net::SSH.start('host', 'username', password: 'password') do |ssh|
  result = ssh.exec!("stat -c %F /path/to/folder")
  # stat -c %F returns "directory" if path is a directory, "file" if path is a file,
  # "symbolic link" if path is a symbolic link, etc.
  puts result
  # => will print "directory" if folder exists on the remote server
end
```

## Zobacz też

- Dokumentacja Ruby o metodzie `Dir.exist?` - https://ruby-doc.org/core-2.6.5/Dir.html#method-c-exist-3F
- Dokumentacja Ruby o metodzie `File.directory?` - https://ruby-doc.org/core-2.6.5/File.html#method-c-directory-3F
- Dokumentacja gemu `net-ssh` - https://net-ssh.github.io/ssh/v2/api/