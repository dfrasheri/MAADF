// Smooth scrolling for internal links
document.querySelectorAll('.nav-link').forEach(link => {
    link.addEventListener('click', function (e) {
      if (this.hash) {
        e.preventDefault();
        const target = document.querySelector(this.hash);
        target.scrollIntoView({ behavior: 'smooth' });
      }
    });
  });

  // Highlight active navbar link on scroll
  const sections = document.querySelectorAll('section');
  const navLinks = document.querySelectorAll('.nav-link');

  window.addEventListener('scroll', () => {
    let current = '';
    sections.forEach(section => {
      const sectionTop = section.offsetTop - 50; // Adjust for header height
      if (window.scrollY >= sectionTop) {
        current = section.getAttribute('id');
      }
    });

    navLinks.forEach(link => {
      link.classList.remove('active');
      if (link.hash === `#${current}`) {
        link.classList.add('active');
      }
    });
  });
  const links = document.querySelectorAll('.nav-link');
  links.forEach(link => {
    link.addEventListener('click', function (e) {
      const currentPage = document.querySelector('.page');
      currentPage.classList.add('slide-out');

      setTimeout(() => {
        window.location.href = this.href;
      }, 500); // Match the transition duration
    });
  });