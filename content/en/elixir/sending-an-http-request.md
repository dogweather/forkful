---
title:                "Elixir recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request is a crucial skill for any Elixir developer. It allows you to communicate with external servers and APIs, making your application more dynamic and flexible. In this blog post, we will explore the basics of sending HTTP requests in Elixir.

## How To

To send an HTTP request in Elixir, we first need to install a library called HTTPoison. This library provides functions to construct and make HTTP requests. We can install it by adding `{:httpoison, "~> 1.0"}` to our `mix.exs` file and running `mix deps.get`.

Next, we need to specify the URL we want to send the request to. In this example, we will use the GitHub API to retrieve information about a user. Our code will look like this:

```Elixir
# Import the HTTPoison library
import HTTPoison

# Specify the URL with the user's username
url = "https://api.github.com/users/johndoe"

# Make the GET request and store the response in a variable
response = get(url)

# Display the response body
IO.puts(response.body)
```

If we run this code in our Elixir console, we will see the information about the user John Doe returned from the GitHub API.

```Elixir
# Output
{
  "login": "johndoe",
  "id": 123456,
  "avatar_url": "https://avatars.githubusercontent.com/u/123456?v=4",
  "url": "https://api.github.com/users/johndoe"
  ...
}
```

We can also send a POST request to the GitHub API to create a new repository. Here is an example of how we can do this using HTTPoison:

```Elixir
# Import the HTTPoison library
import HTTPoison

# Specify the URL for creating a repository
url = "https://api.github.com/user/repos"

# Create a map with the repository name and description
body = %{
  name: "my_awesome_repo",
  description: "This is my first Elixir project"
}

# Make the POST request with the body
response = post(url, body, [{"Authorization", "token <your token>"}])

# Display the response body
IO.puts(response.body)
```

If everything goes well, we will receive a response with the repository information, indicating that our new repository has been created successfully.

```Elixir
# Output
{
  "id": 123456,
  "name": "my_awesome_repo",
  "full_name": "johndoe/my_awesome_repo",
  "html_url": "https://github.com/johndoe/my_awesome_repo"
  ...
}
```

## Deep Dive

Now that we have seen some basic examples of sending HTTP requests in Elixir, let's dive deeper into the topic. HTTPoison offers a variety of functions, such as `put`, `patch`, `delete`, and `head`, to handle different types of requests. It also provides options to customize headers, timeouts, and SSL verification.

Additionally, Elixir has a built-in `HTTP` module that offers a similar set of functions for making HTTP requests. However, HTTPoison is often preferred due to its simpler syntax and added features.

It is essential to handle errors while sending HTTP requests, as they can fail due to various reasons, such as connection issues or incorrect URLs. HTTPoison provides a `handle_error/1` function to handle these errors gracefully.

See Also

- [Elixir School - HTTP Requests](https://elixirschool.com/en/lessons/specifics/http/)
- [HTTPoison Documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Sending HTTP requests in Elixir](https://medium.com/@iammerrick/sending-http-requests-in-elixir-5c5a84304a96)