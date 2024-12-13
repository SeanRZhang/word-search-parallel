import random
import string

def generate_random_word(length):
    return ''.join(random.choice(string.ascii_lowercase) for _ in range(length))

def generate_words(num_words, max_length, min_length):
    return [generate_random_word(random.randint(min_length, max_length)) for _ in range(num_words)]

def create_empty_grid(n, m):
    return [['' for _ in range(m)] for _ in range(n)]

def is_valid_position(grid, x, y):
    return 0 <= x < len(grid) and 0 <= y < len(grid[0]) and grid[x][y] == ''

def get_random_direction():
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # right, down, left, up
    return random.choice(directions)

def place_word_in_grid(grid, word):
    word_len = len(word)
    n, m = len(grid), len(grid[0])

    for _ in range(100):
        start_x = random.randint(0, n - 1)
        start_y = random.randint(0, m - 1)
        dx, dy = get_random_direction()
        x, y = start_x, start_y
        positions = [(x, y)]
        
        for i in range(1, word_len):
            x += dx
            y += dy
            if not (0 <= x < n and 0 <= y < m):
                break
            positions.append((x, y))
        
        if len(positions) == word_len and all(grid[x][y] == '' for x, y in positions):
            for i, (x, y) in enumerate(positions):
                grid[x][y] = word[i]
            return True
    return False

def fill_remaining_cells_with_random_letters(grid):
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == '':
                grid[i][j] = random.choice(string.ascii_lowercase)

def save_grid_and_words_to_file(grid, words, filename):
    with open(filename, 'w') as f:
        f.write(f"{grid}\n")
        f.write(f"{words}\n")

def main():
    n = int(input("Enter the number of rows (n): "))
    m = int(input("Enter the number of columns (m): "))
    num_words = int(input("Enter the number of words: "))
    min_word_length = int(input("Enter the minimum length of each word: "))  
    max_word_length = int(input("Enter the maximum length of each word: "))
    words = generate_words(num_words, max_length=max_word_length, min_length=min_word_length)
    grid = create_empty_grid(n, m)
    for word in words:
        place_word_in_grid(grid, word)
    fill_remaining_cells_with_random_letters(grid)
    filename = input("Enter the output filename (e.g., output.txt): ")
    save_grid_and_words_to_file(grid, words, filename)
    print(f"Grid and words saved to {filename}")

if __name__ == "__main__":
    main()
